# syntax=docker/dockerfile:1

# ############################################################################ #
# Parameters
# ############################################################################ #

ARG GHC_VERSION=9.10.1
ARG GO_VERSION=1.23.0
ARG PROJECT_NAME=test-lurk-hs

# ############################################################################ #
# Runtime Environment
# ############################################################################ #

FROM ubuntu:22.04 AS lurk-hs-runtime

ARG TARGETPLATFORM

RUN <<EOF
  echo "TARGETPLATFORM: $TARGETPLATFORM"
EOF

ARG DEBIAN_FRONTEND=noninteractive
RUN <<EOF
  apt-get update
  apt-get -yqq install \
     --no-install-recommends \
    locales \
    libffi8 \
    libgmp10 \
    libncurses5 \
    libtinfo5
  rm -rf /var/lib/apt/lists/*
  locale-gen en_US.UTF-8
  update-locale LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8
EOF
ENV LANG=en_US.UTF-8

# ############################################################################ #
# Build Environment
# ############################################################################ #

FROM lurk-hs-runtime AS lurk-hs-build-env

ARG BUILDPLATFORM
ARG TARGETPLATFORM
ARG GHC_VERSION=9.10.1
ARG GO_VERSION=1.23.0

ENV CABAL_DIR=/root/.cabal

RUN <<EOF
  echo "BUILDPLATFORM: $BUILDPLATFORM"
  echo "TARGETPLATFORM: $TARGETPLATFORM"
EOF

ARG DEBIAN_FRONTEND=noninteractive
RUN <<EOF
  apt-get update
  apt-get -yqq install \
     --no-install-recommends \
    ca-certificates \
    git \
    neovim \
    curl \
    build-essential \
    binutils \
    pkg-config \
    libclang-dev \
    libffi-dev \
    libgmp-dev \
    libncurses-dev
  # rm -rf /var/lib/apt/lists/*
EOF

# Install GHC
ENV PATH="${PATH}:/root/.local/bin:/root/.ghcup/bin"
ARG BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ARG BOOTSTRAP_HASKELL_MINIMAL=1

RUN --mount=type=cache,target=/root/.ghcup/cache,id=${TARGETPLATFORM} <<EOF
  curl -sSf https://get-ghcup.haskell.org | sh
  ghcup install cabal latest
  ghcup set cabal latest
  ghcup install ghc ${GHC_VERSION}
  ghcup set ghc ${GHC_VERSION}
  cabal --version
  ghc --version
EOF

RUN --mount=type=cache,target=/root/.ghcup/cache,id=${TARGETPLATFORM} <<EOF
  cabal update
EOF

# Install golang
ARG GO_TARGETPLATFORM=${TARGETPLATFORM/\//-}
ARG GOLANG_ARCHIVE=go${GO_VERSION}.${GO_TARGETPLATFORM}.tar.gz
ENV PATH="${PATH}:/usr/local/go/bin"
RUN <<EOF
  curl -L https://go.dev/dl/$GOLANG_ARCHIVE -O
  rm -rf /usr/local/go && tar -C /usr/local -xzf $GOLANG_ARCHIVE
  rm -f go${GO_VERSION}.${GO_TARGETPLATFORM}.tar.gz
  go version
EOF

# Install Rust
ENV PATH="${PATH}:/root/.cargo/bin"
RUN <<EOF
  curl -sSf https://sh.rustup.rs | sh -s -- -y
  curl -LsSf https://get.nexte.st/latest/linux | tar zxf - -C ${CARGO_HOME:-/root/.cargo}/bin
  cargo version
  rustc --version
EOF

# ############################################################################ #
# Build Project
# ############################################################################ #

# ############################################################################ #
# Setup Context

FROM lurk-hs-build-env AS test-lurk-hs-build-ctx
ARG TARGETPLATFORM
ARG PROJECT_NAME
# RUN git clone --filter=tree:0 https://github.com/kadena-io/test-lurk-hs
WORKDIR /${PROJECT_NAME}
COPY . .
ENV GIT_DISCOVERY_ACROSS_FILESYSTEM=1
RUN mkdir -p /tools
COPY <<EOF /tools/check-git-clean.sh
#!/bin/sh
if [ -d ".git" ] && ! [ -f "/tools/wip" ] && ! git diff --exit-code; then \
    echo "Git working tree is not clean. The build changed some file that is checked into git." 1>&2 ; \
    exit 1 ; \
fi
EOF
RUN sh /tools/check-git-clean.sh || touch /tools/wip

# ############################################################################ #
# Build Dependencies

FROM test-lurk-hs-build-ctx AS test-lurk-hs-build-dependencies
ARG TARGETPLATFORM
ARG PROJECT_NAME
ENV GIT_DISCOVERY_ACROSS_FILESYSTEM=1
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=${PROJECT_NAME}-${TARGETPLATFORM},sharing=locked \
    cabal update
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=${PROJECT_NAME}-${TARGETPLATFORM},sharing=locked \
    [ -f cabal.project.freeze ] || cabal --enable-tests --enable-benchmarks freeze
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=${PROJECT_NAME}-${TARGETPLATFORM},sharing=locked \
    cabal build --enable-tests --enable-benchmarks --only-download
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=${PROJECT_NAME}-${TARGETPLATFORM},sharing=locked \
    cabal build --enable-tests --enable-benchmarks --only-dependencies

# ############################################################################ #
# Build Library

FROM test-lurk-hs-build-dependencies AS test-lurk-hs-build-lib
ARG TARGETPLATFORM
ARG PROJECT_NAME
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=${PROJECT_NAME}-${TARGETPLATFORM},sharing=locked \
    cabal build --enable-tests --enable-benchmarks ${PROJECT_NAME}:lib:${PROJECT_NAME}
RUN sh /tools/check-git-clean.sh

# ############################################################################ #
# Build Applications

FROM test-lurk-hs-build-lib AS test-lurk-hs-build-applications
ARG TARGETPLATFORM
ARG PROJECT_NAME
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=${PROJECT_NAME}-${TARGETPLATFORM},sharing=locked \
    cabal build --enable-tests --enable-benchmarks --enable-tests
RUN sh /tools/check-git-clean.sh
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=${PROJECT_NAME}-${TARGETPLATFORM},sharing=locked <<EOF
    mkdir -p /apps/assets
    mkdir -p /apps/verifier-assets
    cp $(cabal list-bin example-lurk-hs) /apps
    cp $(cabal list-bin lurk-hs-tests) /apps
    cp $(cabal list-bin test-indirect) /apps
    cp assets/*.json /apps/assets
    cp verifier-assets/* /apps/verifier-assets
EOF

# ############################################################################ #
# Run Tests Project
# ############################################################################ #

FROM lurk-hs-runtime AS tests-lurk-hs
ARG PROJECT_NAME
ENV PATH=${PATH}:/apps
COPY --from=test-lurk-hs-build-applications /apps/ /apps/
COPY --from=test-lurk-hs-build-applications /${PROJECT_NAME}/LICENSE /apps/LICENSE
COPY --from=test-lurk-hs-build-applications /${PROJECT_NAME}/README.md /apps/README.md
COPY --from=test-lurk-hs-build-applications /${PROJECT_NAME}/cabal.project.freeze /apps/cabal.project.freeze
COPY --chmod=0755 <<EOF /apps/run-tests.sh
#!/bin/sh
/apps/example-lurk-hs
/apps/lurk-hs-tests
/apps/test-indirect
EOF
WORKDIR /apps
RUN /apps/run-tests.sh
ENTRYPOINT ["/apps/run-tests.sh"]


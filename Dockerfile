# syntax=docker/dockerfile:1
FROM ubuntu:22.04

ARG BUILDPLATFORM
ARG TARGETPLATFORM

RUN <<EOF
  echo "BUILDPLATFORM: $BUILDPLATFORM"
  echo "TARGETPLATFORM: $TARGETPLATFORM"
EOF

ARG DEBIAN_FRONTEND=noninteractive
RUN <<EOF
  apt-get update
  apt-get -y install \
    git \
    neovim \
    curl \
    build-essential \
    libffi-dev \
    libffi8 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libncurses5 \
    libtinfo5 \
    pkg-config
EOF

# Install GHC
ENV PATH="${PATH}:/root/.local/bin:/root/.ghcup/bin"
ARG BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ARG BOOTSTRAP_HASKELL_MINIMAL=1
RUN <<EOF
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  ghcup install cabal latest
  ghcup install ghc 9.10.1
  ghcup set ghc 9.10.1
  cabal --version
  ghc --version
EOF

# Install golang
ARG GO_TARGETPLATFORM=${TARGETPLATFORM/\//-}
ARG GOLANG_ARCHIVE=go1.23.0.${GO_TARGETPLATFORM}.tar.gz
ENV PATH="${PATH}:/usr/local/go/bin"
RUN <<EOF
  curl -L https://go.dev/dl/$GOLANG_ARCHIVE -O
  rm -rf /usr/local/go && tar -C /usr/local -xzf $GOLANG_ARCHIVE
  rm -f go1.23.0.${GO_TARGETPLATFORM}.tar.gz
  go version
EOF

# Install Rust
ENV PATH="${PATH}:/root/.cargo/bin"
RUN <<EOF
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
  curl -LsSf https://get.nexte.st/latest/linux | tar zxf - -C ${CARGO_HOME:-/root/.cargo}/bin
  cargo version
  rustc --version
EOF

RUN git clone https://github.com/kadena-io/test-lurk-hs.git

WORKDIR /test-lurk-hs

# build Rust library
# RUN cargo build --release
# RUN cargo nextest run --release --no-fail-fast

# build Haskell package
RUN cabal update
RUN cabal build --enable-tests --dependencies-only
RUN cabal build
RUN cabal test


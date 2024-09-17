{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Verify
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Verify
( verifierName
, supportedMachineVersions
, defaultMachineVersion
, runExample
, ProofClaim(..)
, claimProgramId
, claimParameters
, claimProof
, claimMachineVersion
, readProof
) where

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Aeson.Types (JSONPathElement(Key))
import Data.ByteString.Short qualified as BS

import Lens.Micro.TH

-- Plonk

import EmbedVKeys
import PlonkVerify

-- internal modules

import Utils

-- -------------------------------------------------------------------------- --

verifierName :: String
verifierName = "PLONK_BN254"

defaultMachineVersion :: String
defaultMachineVersion = "v1.0.8-testnet"

supportedMachineVersions :: [(FilePath, VKey)]
supportedMachineVersions = $$(embedVKeys "vk" "verifier-assets")

-- -------------------------------------------------------------------------- --
-- Orphans

deriving via HexEncoded instance FromJSON Proof
deriving via HexEncoded instance FromJSON ProgramId
deriving via HexEncoded instance FromJSON PublicParameter

instance FromJSON PublicParameterHash where
    parseJSON = parseJSON >=> \x -> case mkPublicParameterHash (_hexEncoded x) of
        Right r -> return r
        Left e -> fail $ "invalid public parameter hash bytes: " <> e

-- -------------------------------------------------------------------------- --
-- Test Proof Claims

data ProofClaim = ProofClaim
    { _claimProgramId :: !ProgramId
        -- ^ Identifies the RISC-V program that is proven. A program is valid
        -- only in the context of a particular verifying key. Each program has a
        -- well defined set of public parameters.

    , _claimParameters :: !(Either PublicParameterHash PublicParameter)
        -- ^ The public parameters of the respective program. For verification
        -- the parameters are encoded and hashed.
        --
        -- In the context of this test suite a the length is used as heuristics
        -- for whether the value is a digest or a list.

    , _claimProof :: !Proof
        -- ^ The actual proof object.

    , _claimMachineVersion :: !String
        -- ^ The version of the virtual machine on which the program was
        -- executed.
    }

-- Test that template Haskell works, which depends on dynamic linking.
--
makeLenses ''ProofClaim

instance FromJSON ProofClaim where
    parseJSON = withObject "ProofClaim" $ \o -> ProofClaim
        <$> o .: "programId"
        <*> parseParameters o
        <*> o .: "proof"
        <*> o .:? "machineVersion" .!= defaultMachineVersion
      where
        parseParameters o
            = (Left <$> o .: "parametersHash")
            <|> (Right <$> o .: "parameters")
            <?> Key "parameters|parametersHash"

readProof :: String -> IO ProofClaim
readProof name = eitherDecodeFileStrict' ("./assets/" <> name <> ".json") >>= \case
    Left e -> fail $ "failed to load proof " <> name <> ": " <> e
    Right p -> return p

-- --------------------------------------------------------------------------
-- Run Example

runExample :: String -> IO Bool
runExample name = do
    p <- readProof name
    case lookup (_claimMachineVersion p) supportedMachineVersions of
        Just v -> do
            case _claimParameters p of
                Left pp -> verifyPlonkBn254' v
                    (_claimProof p)
                    (_claimProgramId p)
                    pp
                Right pp -> verifyPlonkBn254 v
                    (_claimProof p)
                    (_claimProgramId p)
                    pp
        Nothing -> error $ "unsupported machine version: " <> (_claimMachineVersion p)


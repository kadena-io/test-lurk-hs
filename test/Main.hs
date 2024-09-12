{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module: Main
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Main
( main
) where

import Data.Aeson

import Foreign.C.String

import Test.Hspec

-- internal modules

import PlonkVerify

-- -------------------------------------------------------------------------- --
-- main

main :: IO ()
main = hspec $ describe "examples" $ do
    testExample "fibonacci_fixture"
    testExample "epoch_change"
    testExample "inclusion_fixture"

-- --------------------------------------------------------------------------
-- Run Example

testExample :: String -> SpecWith ()
testExample name = it name $ do
    r <- runExample name
    shouldNotBe r 0

runExample :: String -> IO Int
runExample name = do
    p <- readProof name
    dataDir <- newCString "./verifier-assets/v1.0.8-testnet"
    proof <- newCString (_proofProof p)
    vkeyHash <- newCString (_proofVKey p)
    committedValuesDigest <- newCString (_proofPublicValues p)

    fromIntegral <$> verify_plonk_bn254
        dataDir
        proof
        vkeyHash
        committedValuesDigest

-- -------------------------------------------------------------------------- --
-- Utils

data Proof = Proof
    { _proofVKey :: !String
    , _proofPublicValues :: !String
    , _proofProof :: !String
    }

instance FromJSON Proof where
    parseJSON = withObject "Proof" $ \o -> Proof
        <$> o .: "vkey"
        <*> o .: "publicValues"
        <*> o .: "proof"

readProof :: String -> IO Proof
readProof name = eitherDecodeFileStrict' ("./assets/" <> name <> ".json") >>= \case
    Left e -> fail $ "failed to load proof " <> name <> ": " <> e
    Right p -> return p



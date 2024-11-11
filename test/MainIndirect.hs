{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Main
( main
, proofProof
, proofPublicValues
, proofPKey
) where

import Lens.Micro.TH

import Test.Hspec

-- internal modules

import Verify

-- -------------------------------------------------------------------------- --
-- Utils

data Proof = Proof
    { _proofPKey :: !String
    , _proofPublicValues :: !String
    , _proofProof :: !String
    }

-- Test that template Haskell works, which depends on dynamic linking.
--
makeLenses ''Proof

-- -------------------------------------------------------------------------- --
-- main

main :: IO ()
main = hspec $ describe "examples" $ do
    testSuccess "fibonacci_fixture"
    testSuccess "epoch_change"
    testSuccess "inclusion_fixture"
    testFailure "empty"
  where
    testSuccess n = testExample n True
    testFailure n = testExample n False

-- --------------------------------------------------------------------------
-- Run Example

testExample :: String -> Bool -> SpecWith ()
testExample name expected = it name $ do
    r <- runExample name
    shouldBe r expected


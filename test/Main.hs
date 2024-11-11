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

import Test.Hspec

-- plonk verifier

import PlonkBn254.Verify

-- internal modules

import Verify hiding (runExample)

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

-- Test the case when 'verifyPlonkBn254' is imported directly by the component.
--
runExample :: String -> IO Bool
runExample name = do
    p <- readProof name
    case lookup (_claimMachineVersion p) supportedMachineVersions of
        Just v -> do
            case _claimParameters p of
                Left pp -> verifyPrehashed v
                    (_claimProof p)
                    (_claimProgramId p)
                    pp
                Right pp -> verify v
                    (_claimProof p)
                    (_claimProgramId p)
                    pp
        Nothing -> error $ "unsupported machine version: " <> (_claimMachineVersion p)

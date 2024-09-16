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

-- internal modules

import Verify

-- -------------------------------------------------------------------------- --
-- main

main :: IO ()
main = do
    runExample "fibonacci_fixture" >>= print
    runExample "epoch_change" >>= print
    runExample "inclusion_fixture" >>= print

-- Inclusion proof parameters
-- {
--   "blockHeight": "e0fc910000000000",
--   "committeeHash": "0969ed235cf75d25800ea6845c2584af013c1f9617ad2de87202d7e9b93739c9",
--   "proofAddress": "5c69bee701ef814a2b6a3edd4b1652cb9cc5aa6f",
--   "proofAddressHash": "22002fe30a172d0a479f6add89c63b29dce29b6071b3c7e486b0fb4bc431f885",
--   "count": "0100000000000000",
--   "storage": [
--     {
--       "key": "2000000000000000290decd9548b62a8ef0d3e6ac11e2d7b95a49e22ecf57fc6",
--       "value": "044b6f007ca2b2ba010000000000000080"
--     }
--   ]
-- }

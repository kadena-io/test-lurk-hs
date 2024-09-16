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
-- Module: Utils
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Utils
( HexEncoded(..)
, HexEncodedFixed(..)
) where

import Control.Monad

import Data.Aeson
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Short qualified as BS
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import GHC.Exts (proxy#)
import GHC.TypeNats

-- -------------------------------------------------------------------------- --
-- Utils

-- | Helper for hex encodings
--
newtype HexEncoded = HexEncoded
    { _hexEncoded :: BS.ShortByteString }
    deriving (Show, Eq, Ord)

instance FromJSON HexEncoded where
    parseJSON = withText "HexEncoded" $ \str -> do
        let sstr = fromMaybe str $ T.stripPrefix "0x" str
        case B16.decode (T.encodeUtf8 sstr) of
           Left e -> fail $ "decodeing hex string failed: " <> e
           Right bytes -> return (HexEncoded $ BS.toShort bytes)

newtype HexEncodedFixed (n :: Natural) = HexEncodedFixed
    { _hexEncodedFixed :: BS.ShortByteString }
    deriving (Show, Eq, Ord)

instance KnownNat n => FromJSON (HexEncodedFixed n) where
    parseJSON v = do
        (HexEncoded x) <- parseJSON v
        unless (BS.length x == expectedLength) $ do
            fail $ "HexEncodedFixed: wrong length. Expected: " <> show expectedLength <> ". Actual: " <> show (BS.length x)
        return $ HexEncodedFixed x
      where
        expectedLength = fromIntegral $ natVal' (proxy# @n)


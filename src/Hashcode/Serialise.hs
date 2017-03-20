-- |
-- Module      : Hashcode.Logic
-- Description : Defines serialisation logic for the output `Solution` type
-- Copyright   : (c) Jonatan H Sundqvist and Jayant Shivarajan, 2017
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : Portable
--0

-- TODO | -
--        -

-- SPEC | -
--        -
-- GHC Directives -------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields #-}

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Hashcode.Serialise where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import           Data.Foldable   (toList)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.Set        as Set

import Hashcode.Types

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- |
serialise :: Solution -> ByteString
serialise solution = BS.pack . unlines $ cacheCount : cacheServers
  where
    cacheCount :: String
    cacheCount   = show . length $ caches (solution :: Solution)

    cacheServers :: [String]
    cacheServers = map showCache (toList $ caches (solution :: Solution))

    showCache :: CacheContents -> String
    showCache c  = unwords $ (show . unID $ uuid (c :: CacheContents)) : map (show . unID . uuid) (Set.elems $ videos (c :: CacheContents))

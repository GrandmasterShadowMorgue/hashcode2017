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
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)
import qualified Data.Map        as Map
import qualified Data.Set        as Set

import Hashcode.Types

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- | Example solution for testing
dummySolution :: Solution
dummySolution = Solution . Map.fromList $ map makeCacheContents cacheDescriptions
  where
    makeCacheContents (icache, ivideos) = (ID icache, CacheContents . Set.fromList $ map ID ivideos)
    cacheDescriptions = [(0, [0,2,5,3,8]),
                         (3, [6,2,1]),
                         (6, [2,1,3]),
                         (2, [1,9])]


-- |
serialise :: Solution -> ByteString
serialise solution = BS.pack . unlines $ showCacheCount : showCacheServers
  where
    showCacheCount :: String
    showCacheCount = show . length $ caches (solution :: Solution)

    showCacheServers :: [String]
    showCacheServers = map (uncurry showCacheServer) . Map.toList $ caches (solution :: Solution)

    showCacheServer :: (ID Cache) -> CacheContents -> String
    showCacheServer i c = unwords . map show $ unID i : map unID (Set.elems $ videos (c :: CacheContents))

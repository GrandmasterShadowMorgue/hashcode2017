-- |
-- Module      : Hashcode.Types
-- Description : Defines the types for our problem domain
-- Copyright   : (c) Jonatan H Sundqvist and Jayant Shivarajan, 2017
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : Portable
--

-- TODO | -
--        -

-- SPEC | -
--        -

-- GHC Directivies -------------------------------------------------------------------------------------------------------------------------

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Hashcode.Types where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import Data.Map    (Map)
import Data.Vector (Vector)

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- | A typesafe wrapper for storage size
newtype Megabytes = Megabytes Int deriving (Eq, Ord, Show)


-- | A typesafe wrapper for time
newtype Milliseconds = Milliseconds Int deriving (Eq, Ord, Show)


-- | Represents an ID for a value of type `a`
data ID a = ID { id :: Int } deriving (Eq, Show)


-- | Represents a video
data Video = Video {
  uuid :: ID Video,
  size :: Megabytes
} deriving (Eq, Show)


-- | Represents a network endpoint
data Endpoint = Endpoint {
  latency :: Milliseconds,
  caches  :: Map (ID Cache) Millisecond
} deriving (Eq, Show)


-- | Represents a cache server (hmmm....)
data Cache = Cache { uuid :: ID Cache } deriving (Eq, Show)


-- | Represents a set of requests to the same video from the same endpoint
data Request = Request { video :: ID Video, endpoint :: ID Endpoint, amount :: Int }


-- | Describes the entire network of endpoints, videos and cache servers
data Network = Network {
  cacheCapacity :: Megabytes,
  videos    :: Vector Video,
  endpoints :: Vector Endpoint,
  caches    :: Vector Cache,
  requests  :: Vector Request
} deriving (Eq, Ord, Show)

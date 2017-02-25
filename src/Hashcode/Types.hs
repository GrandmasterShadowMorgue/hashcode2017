module Hashcode.Types where

newtype Megabytes = Megabytes Int

newtype Milliseconds = Milliseconds Int

data ID a = ID { id :: Int }

data Video = Video { size :: Megabytes } deriving (Show, Eq, Ord)

data Endpoint = Endpoint { latencyToDC :: Milliseconds, caches :: Int, cacheLatencies :: HashMap ID Cache Milliseconds}

data Cache = Cache { capacity :: Megabytes} deriving (Show, Eq, Ord)

data Request = Request { video :: ID Video, endpoint :: ID Endpoint, amount :: Int }

data Network = Network {videos :: Vector Video, endpoints :: Vector Endpoint, caches :: Vector Cache, requests :: Vector Request }

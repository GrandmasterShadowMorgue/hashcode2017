-- |
-- Module      : Hashcode.Parse
-- Description : Defines the parser and sub-parsers for the raw input data
-- Copyright   : (c) Jonatan H Sundqvist and Jayant Shivarajan, 2017
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist, Jayant Shivarajan
-- Stability   : experimental
-- Portability : Portable
--

-- TODO | -
--        -

-- SPEC | -
--        -

-- GHC Directives --------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields #-}

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Hashcode.Parse where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Applicative ((<$>), (<*>), liftA2)

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as BS

import Hashcode.Types (Video(Video), Endpoint(Endpoint), Request(Request), ID(ID), Cache(..), Megabytes(..), Milliseconds(..), Network(Network))

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Demonstrate another cool way (recursive) of writing this parser that doesn't involve appending the last element
videos :: Int -> Atto.Parser (Vector Video)
videos n = Vector.fromList . sizesToVideos <$> liftA2 (:) megabytes (remaining <* Atto.char '\n')
  where
    sizesToVideos :: [Megabytes] -> [Video]
    sizesToVideos = zipWith (Video . ID) [0..]

    remaining :: Atto.Parser [Megabytes]
    remaining = Atto.count (n-1) (Atto.char ' ' *> megabytes)


-- |
endpoints :: Int -> Atto.Parser (Vector Endpoint)
endpoints n = _
  where
    cache :: Atto.Parser (ID Cache, Milliseconds)
    cache    = (,) <$> (identifier <* Atto.char ' ') <*> (milliseconds <* Atto.char '\n')

    endpoint :: Atto.Parser Endpoint
    endpoint = do
      latency <- milliseconds <* Atto.char ' '
      nCaches <- Atto.decimal <* Atto.char '\n'
      Atto.count nCaches cache


-- |
requests :: Int -> Atto.Parser (Vector Request)
requests n = _


-- |
megabytes :: Atto.Parser Megabytes
megabytes = Megabytes <$> Atto.decimal


-- |
milliseconds :: Atto.Parser Milliseconds
milliseconds = Milliseconds <$> Atto.decimal


-- |
identifier :: Atto.Parser (ID a)
identifier = ID <$> Atto.decimal


-- |
network :: Atto.Parser Network
network = do
  videoCount    <- Atto.decimal <* Atto.char ' '
  endpointCount <- Atto.decimal <* Atto.char ' '
  requestCount  <- Atto.decimal <* Atto.char ' '
  cacheCount    <- Atto.decimal <* Atto.char ' '
  Network
    <$> (megabytes <* Atto.char '\n') -- Cache capacity
    <*> (videos videoCount)           -- Videos
    <*> (endpoints endpointCount)     -- Endpoints
    <*> (requests cacheCount)         -- Requests

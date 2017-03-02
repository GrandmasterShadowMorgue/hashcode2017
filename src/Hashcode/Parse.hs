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
import Control.Applicative ((<$>), (<*>))

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as BS

import Hashcode.Types (Video(Video), Endpoint(Endpoint), Request(Request), ID(..), Megabytes(..), Milliseconds(..), Network(Network))

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- |
videos :: Int -> Atto.Parser (Vector Video)
videos n = do
  prefix <- Atto.count (n-1) (megabytes <* Atto.char ' ')
  suffix <- megabytes <* Atto.char '\n'
  pure . Vector.fromList $ zipWith (\i mb -> Video (ID i) mb) [0..] (prefix ++ [suffix]){- take each shizzle from da numbuh list as da id arg-}


-- |
endpoints :: Int -> Atto.Parser (Vector Endpoint)
endpoints n = _


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

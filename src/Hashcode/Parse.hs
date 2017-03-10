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

module Hashcode.Parse (fromBytestring, network) where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import Data.Vector (Vector)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Control.Applicative ((<$>), (<*>), liftA2)

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.Attoparsec.Combinator       ((<?>))
import qualified Data.ByteString                  as BS

import Hashcode.Types (Video(Video), Endpoint(Endpoint), Request(Request), ID(ID), Cache(..), Megabytes(..), Milliseconds(..), Network(Network))

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- | Applies the network parser to a `ByteString`. Saves us from having to import Attoparsec along with this module.
fromBytestring :: BS.ByteString -> Either String Network
fromBytestring = Atto.parseOnly network -- Atto.eitherResult


-- | Parses `n` occurrences of `p`, separated by `sep`
-- TODO | - Rename (?)
countedSepBy :: Int -> Atto.Parser a -> Atto.Parser b -> Atto.Parser [a]
countedSepBy 0 _ _   = pure []
countedSepBy n p sep = liftA2 (:) p (Atto.count (n-1) (sep *> p))


-- | Parses `n` video definitions. Negative counts are treated as though `n` were zero.
-- TODO | - Demonstrate another cool way (recursive) of writing this parser that doesn't involve appending the last element (✓)
videos :: Int -> Atto.Parser (Vector Video)
videos 0 = pure Vector.empty
videos n = Vector.fromList . sizesToVideos <$> (sizes <* Atto.endOfLine)
  where
    sizesToVideos :: [Megabytes] -> [Video]
    sizesToVideos = zipWith (Video . ID) [0..]

    sizes :: Atto.Parser [Megabytes]
    sizes = countedSepBy n megabytes (Atto.char ' ')


-- | Parses `n` endpoint definitions. Negative counts are treated as though `n` were zero.
endpoints :: Int -> Atto.Parser (Vector Endpoint)
endpoints n = Vector.fromList <$> Atto.count n endpoint
  where
    cache :: Atto.Parser (ID Cache, Milliseconds)
    cache    = (,) <$> (identifier <* Atto.char ' ') <*> (milliseconds <* Atto.endOfLine)

    endpoint :: Atto.Parser Endpoint
    endpoint = do
      latency <- milliseconds <* Atto.char ' '
      nCaches <- Atto.decimal <* Atto.endOfLine
      Endpoint latency . Map.fromList <$> Atto.count nCaches cache


-- |
requests :: Int -> Atto.Parser (Vector Request)
requests 0 = pure Vector.empty
requests n = Vector.fromList <$> countedSepBy n request (Atto.endOfLine)
  where
    request :: Atto.Parser Request
    request = Request
                <$> (identifier <* Atto.char ' ')
                <*> (identifier <* Atto.char ' ')
                <*> (Atto.decimal)


-- | Parses an unsigned decimal number, treating it as a Megabytes value.
megabytes :: Atto.Parser Megabytes
megabytes = Megabytes <$> Atto.decimal


-- | Parses an unsigned decimal number, treating it as a Milliseconds value.
milliseconds :: Atto.Parser Milliseconds
milliseconds = Milliseconds <$> Atto.decimal


-- | Parses an unsigned decimal number, treating it as an Identifier value.
identifier :: Atto.Parser (ID a)
identifier = ID <$> Atto.decimal


-- | Parses a `Network` definition.
network :: Atto.Parser Network
network = do
  videoCount    <- Atto.decimal <* Atto.char ' '
  endpointCount <- Atto.decimal <* Atto.char ' '
  requestCount  <- Atto.decimal <* Atto.char ' '
  Network
    <$> (Atto.decimal <* Atto.char ' ')  -- Cache count (should we care?)
    <*> (megabytes    <* Atto.endOfLine) -- Cache capacity
    <*> (videos videoCount)              -- Videos
    <*> (endpoints endpointCount)        -- Endpoints
    <*> (requests requestCount)          -- Requests
    <*  (Atto.skipSpace <* Atto.endOfInput)

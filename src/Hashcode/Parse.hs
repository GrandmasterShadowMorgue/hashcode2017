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

import Control.Applicative ((<$>), (<*>))

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as BS

import Hashcode.Types

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- |
video :: Int -> Atto.Parser (Vector Video)
video n = _


-- |
endpoint :: Int -> Atto.Parser (Vector Endpoint)
endpoint n = _


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
uuid :: Atto.Parser (ID a)
uuid = ID <$> Atto.decimal


-- |
network :: Atto.Parser Network
network = do
  videoCount    <- Atto.decimal <* Atto.char ' '
  endpointCount <- Atto.decimal <* Atto.char ' '
  requestCount  <- Atto.decimal <* Atto.char ' '
  cacheCount    <- Atto.decimal <* Atto.char ' '
  Network <$>
    megabytes <*> -- Cache capacity
    videos    <*> -- Videos
    endpoints <*> -- Endpoints
    requests  <*> -- Requests

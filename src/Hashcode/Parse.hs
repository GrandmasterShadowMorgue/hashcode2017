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

-- GHC Directives -------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields #-}

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Hashcode.Parse where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as BS

import Hashcode.Types

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

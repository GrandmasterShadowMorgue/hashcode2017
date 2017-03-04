-- |
-- Module      : Hashcode.App
-- Description : Ties all the knots together
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

-- GHC Directives -------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields #-}

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Hashcode.App where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import Control.Exception

import System.FilePath
import System.Directory

import Hashcode.Types
import Hashcode.Parse
import Hashcode.Logic
import Hashcode.Serialise

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

app :: IO ()
app = do
  return ()

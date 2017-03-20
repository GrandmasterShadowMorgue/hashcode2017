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

import qualified Prelude

import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)

import Hashcode.Types

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- |
-- serialise :: Solution -> ByteString
-- serialise = _

-- |
-- Module      : Hashcode.Logic
-- Description : Defines the core optimisation logic
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

module Hashcode.Logic where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import Data.Set              (Set)
import qualified Data.Set    as Set
import Data.Map              (Map)
import qualified Data.Map    as Map
import           Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector

import Hashcode.Types

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- |
index :: Vector a -> ID a -> Maybe a
index v i = v !? unID i


-- |
score :: Network -> Solution -> Score
score network solution = _


-- |
solve :: Network -> Solution
solve network = _

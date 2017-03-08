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
{-# LANGUAGE TemplateHaskell       #-}

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Hashcode.App where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import Data.FileEmbed (makeRelativeToProject, strToExp)

import Control.Exception

import System.FilePath  (takeExtension)
import System.Directory (getDirectoryContents)

import Hashcode.Types
import Hashcode.Parse
import Hashcode.Logic
import Hashcode.Serialise

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- | Lists the paths of all datasets in the given folder.
enumerateDatasets :: FilePath -> IO [FilePath]
enumerateDatasets folder = filter (hasExtension ".in") <$> (getDirectoryContents $ makeAbsolute folder)
  where
    hasExtension ext = (== ext) . takeExtension
    makeAbsolute fn  = $(makeRelativeToProject fn >>= strToExp)


-- |
loadDataset :: FilePath -> IO (Either String Network)
loadDataset fn = _


-- |
app :: IO ()
app = do
  datasets <- filter ((== ".in") . takeExtension) <$> getDirectoryContents $(makeRelativeToProject "data/input/" >>= strToExp)
  mapM print datasets
  return ()

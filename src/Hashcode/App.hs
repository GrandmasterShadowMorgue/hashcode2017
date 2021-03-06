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
{-# LANGUAGE TupleSections         #-}

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Hashcode.App where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)
import           Data.Bifunctor  (first, second)

import           Data.FileEmbed (makeRelativeToProject, strToExp)
import qualified Data.Time.Clock as Clock

import Text.Printf (printf)

import           Control.Exception
import qualified Control.Concurrent.Async as Async

import System.FilePath  (takeExtension, takeBaseName, normalise, (</>))
import System.Directory (getDirectoryContents)

import Hashcode.Types
import Hashcode.Parse
import Hashcode.Logic
import Hashcode.Serialise

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- | Lists the paths of all datasets in the given folder.
enumerateDatasets :: FilePath -> IO (Either String [FilePath])
enumerateDatasets folder = second (map (robustPath . (folder </>)) . prune) <$> catch everyPath explain
  where
    prune :: [FilePath] -> [FilePath]
    prune = filter ((== ".in") . takeExtension)

    robustPath :: FilePath -> FilePath
    robustPath fn  = normalise $ $(makeRelativeToProject "." >>= strToExp) </> fn

    explain :: SomeException -> IO (Either String [FilePath])
    explain e = return . Left $ show (folder, e)

    everyPath :: IO (Either String [FilePath])
    everyPath = Right <$> getDirectoryContents (robustPath folder)


-- |
-- TODO | Only catch IO exceptions (?)
loadDataset :: FilePath -> IO (Either String Network)
loadDataset fn = catch load explain
  where
    explain e = return . Left $ show (fn, e :: SomeException)
    load      = BS.readFile fn >>= return . first (show . (takeBaseName fn,)) . fromBytestring


-- |
loadDatasetsFrom :: FilePath -> IO (Either String [Either String Network])
loadDatasetsFrom fn = do
  epaths <- enumerateDatasets fn
  either (return . Left) (fmap Right . Async.mapConcurrently loadDataset) epaths


-- |
app :: IO ()
app = flip catch (\e -> print (e :: SomeException)) $ do
  datasets <- enumerateDatasets "data/input/"
  mapM print datasets
  either
    (const $ putStrLn "Sorry")
    (mapM_ (\fn -> BS.readFile fn >>= \s -> putStrLn $ printf "%s has %d lines." (takeBaseName fn) (BS.count '\n' s)))
    (datasets)

  t <- Clock.getCurrentTime
  datasets <- loadDatasetsFrom "data/input/"
  either
    (\e -> putStrLn $ "Something went wrong: " ++ e)
    (mapM_ $ putStrLn . take 140 . show)
    (datasets)
  t' <- Clock.getCurrentTime
  print $ Clock.diffUTCTime t' t
  return ()

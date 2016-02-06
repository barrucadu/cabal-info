{-# LANGUAGE TupleSections #-}

module Main where

import Control.Exception (SomeException, catch)
import Control.Monad (unless)

import Data.Maybe (fromMaybe)

import Distribution.PackageDescription (GenericPackageDescription, PackageDescription)
import Distribution.System (buildArch, buildOS)

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Args
import Describe
import Fields

import Cabal.Info

main :: IO ()
main = do
  args  <- getArgs
  gpkgd <- getGenericPackageDescription args
  let pkgd = do
        (g, fp) <- gpkgd
        p <- applyConfiguration args fp g
        pure (g, p)

  case (pkgd, field args) of
    (Right pkg, Just fld) ->
      let fldval = getField fld pkg
      in unless (null fldval) $ putStrLn fldval
    (Right (_, pkg), Nothing) -> putStrLn $ describe pkg
    (Left err, _) -> dieWith $ prettyPrintErr err

-- | Attempt to fetch and parse the package description.
getGenericPackageDescription :: Args -> IO (Either CabalError (GenericPackageDescription, FilePath))
getGenericPackageDescription args = case cabalFile args of
  Just fp -> fmap (,fp) <$> openGenericPackageDescription fp
  Nothing -> findGenericPackageDescription

-- | Evaluate the conditionals in the package description.
applyConfiguration :: Args -> FilePath -> GenericPackageDescription -> Either CabalError PackageDescription
applyConfiguration args = evaluateConditions (flags args) os arch . Just where
  os   = Just . fromMaybe buildOS   $ theOS   args
  arch = Just . fromMaybe buildArch $ theArch args

-- | Print a message to stderr and exit with failure.
dieWith :: String -> IO ()
dieWith err = hPutStrLn stderr err >> exitFailure

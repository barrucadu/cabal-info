module Main where

import Control.Exception (SomeException, catch)
import Control.Monad (unless)

import Data.Maybe (fromMaybe)

import Distribution.PackageDescription (PackageDescription)
import Distribution.System (buildArch, buildOS)

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Args
import Describe
import Fields

import Cabal.Info

main :: IO ()
main = do
  args <- getArgs
  pkgd <- getPackageDescription args

  case (pkgd, field args) of
    (Right pkg, Just fld) ->
      let fldval = getField fld pkg
      in unless (null fldval) $ putStrLn fldval
    (Right pkg, Nothing) -> putStrLn $ describe pkg
    (Left err, _) -> dieWith $ prettyPrintErr err

-- | Attempt to fetch and parse the package description.
getPackageDescription :: Args -> IO (Either CabalError PackageDescription)
getPackageDescription args = maybe (fmap fst <$> findDefault) parseFile $ cabalFile args where
  parseFile fp = openPackageDescription' fp (flags args) os arch
  findDefault  = findPackageDescription'    (flags args) os arch

  os   = Just . fromMaybe buildOS   $ theOS   args
  arch = Just . fromMaybe buildArch $ theArch args

-- | Print a message to stderr and exit with failure.
dieWith :: String -> IO ()
dieWith err = hPutStrLn stderr err >> exitFailure

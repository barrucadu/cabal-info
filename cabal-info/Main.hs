module Main where

import Control.Exception (SomeException, catch)
import Control.Monad (unless)

import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Verbosity (silent)

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
    (Left err, _) -> dieWith err

-- | Attempt to fetch and parse the package description.
getPackageDescription :: Args -> IO (Either String PackageDescription)
getPackageDescription args = maybe (fmap fst <$> findDefault) parseFile $ cabalFile args where
  parseFile fp = openPackageDescription' fp (flags args) Nothing Nothing
  findDefault  = findPackageDescription'    (flags args) Nothing Nothing

-- | Print a message to stderr and exit with failure.
dieWith :: String -> IO ()
dieWith err = hPutStrLn stderr err >> exitFailure

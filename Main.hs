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

main :: IO ()
main = do
  args <- getArgs
  pkgd <- getPackageDescription args

  case (pkgd, field args) of
    (Right pkg, Just fld) ->
      let fldval = trim $ getField fld pkg
      in unless (null fldval) $ putStrLn fldval
    (Right pkg, Nothing) -> putStrLn . trim $ describe pkg
    (Left err, _) -> dieWith err

-- | Attempt to fetch and parse the package description.
getPackageDescription :: Args -> IO (Either String PackageDescription)
getPackageDescription args = go (cabalFile args) `catchAll` (\_ -> pure $ Left "Failed to read .cabal file") where
  go (Just cfile) = do
    -- TODO: get the actual platform and compiler version.
    let platform = buildPlatform
    let compiler = unknownCompilerInfo buildCompilerId NoAbiTag
    pkgdesc <- finalizePackageDescription (flags args) (const True) platform compiler [] <$> readPackageDescription silent cfile
    pure $
      case pkgdesc of
        Right (pkgdesc', _) -> Right pkgdesc'
        _ -> Left "Could not find successful flag assignment."

  go Nothing = pure $ Left "Could not find .cabal file."

  catchAll :: IO a -> (SomeException -> IO a) -> IO a
  catchAll = catch

-- | Drop leading and trailing blank lines.
trim :: String -> String
trim = reverse . dropWhile (`elem`"\n\r") . reverse . dropWhile (`elem`"\n\r")

-- | Print a message to stderr and exit with failure.
dieWith :: String -> IO ()
dieWith err = hPutStrLn stderr err >> exitFailure

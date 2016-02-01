module Main where

import Control.Exception (SomeException, catch)

import Data.Maybe (maybeToList)

import Distribution.Compiler
import Distribution.License
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Text (display)
import Distribution.Verbosity
import Distribution.Version

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Args

main :: IO ()
main = do
  args <- getArgs
  pkgd <- getPackageDescription args

  case (pkgd, field args) of
    (Left err, _) -> dieWith err
    (Right pkg, Just fld) -> mapM_ putStrLn . trim $ getField fld pkg
    _ -> pure ()

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
trim :: [String] -> [String]
trim = reverse . dropWhile (=="") . reverse . dropWhile (=="")

-- | Print a message to stderr and exit with failure.
dieWith :: String -> IO ()
dieWith err = hPutStrLn stderr err >> exitFailure

-- | Get a field from a package description, returning a list of
-- values. The empty list indicates that either the field was present,
-- but contained nothing, or the field was not present.
--
-- There are a number of parts of the package description not yet
-- exposed. Doing this nicely may require more than a single-word
-- field name eg, to specify which executable or test suite is being
-- referred to.
--
-- TODO: testedWith
-- TODO: sourceRepos
-- TODO: buildDepends
-- TODO: library reexportedModules
-- TODO: library requiredSignatures
-- TODO: library exposedSignatures
-- TODO: library libBuildInfo (all except other-modules)
-- TODO: executables
-- TODO: testSuites
-- TODO: benchmarks
-- TODO: dataFiles
-- TODO: dataDir
-- TODO: extraSrcFiles
-- TODO: extraTmpFiles
-- TODO: extraDocFiles
getField :: String -> PackageDescription -> [String]
getField "name"    = (:[]) . unPackageName . pkgName . package
getField "version" = (:[]) . display . pkgVersion . package
getField "license" = (:[]) . display . license
getField "license-file" = licenseFiles
getField "license-files" = licenseFiles
getField "copyright" = (:[]) . copyright
getField "maintainer" = (:[]) . maintainer
getField "author" = (:[]) . author
getField "stability" = (:[]) . stability
getField "homepage" = (:[]) . homepage
getField "package-url" = (:[]) . pkgUrl
getField "bug-reports" = (:[]) . bugReports
getField "synopsis" = (:[]) . synopsis
getField "description" = (:[]) . description
getField "category" = (:[]) . category
getField "build-type" = map display . maybeToList . buildType
getField "exposed-modules" = map display . maybe [] exposedModules . library
getField "exposed" = maybe [] ((:[]) . display . libExposed) . library
getField "other-modules" = map display . maybe [] (otherModules . libBuildInfo) . library
getField _ = const []

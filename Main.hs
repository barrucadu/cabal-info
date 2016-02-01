module Main where

import Data.Maybe (maybeToList)

import Distribution.License
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Text (display)
import Distribution.Verbosity
import Distribution.Version

import Args

main :: IO ()
main = do
  args <- getArgs

  case (cabalFile args, field args) of
    (Just cfile, Just fld) -> do
      -- TODO: Handle exceptions.
      pkgdesc <- flattenPackageDescription <$> readPackageDescription silent cfile
      mapM_ putStrLn $ getField fld pkgdesc
    (Nothing, _) -> putStrLn "Could not find .cabal file."
    _ -> return ()

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

-- | Accessing fields from packages.
module Fields
  ( FieldName(..)
  , getField
  ) where

import Data.Maybe (maybeToList)

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text (display)
import Distribution.Version

-- | A field name is (currently) just a string.
newtype FieldName = FieldName String
  deriving Show

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
getField :: FieldName -> PackageDescription -> [String]
-- Generic
getField (FieldName "name")          = (:[]) . unPackageName . pkgName . package
getField (FieldName "version")       = (:[]) . display . pkgVersion . package
getField (FieldName "license")       = (:[]) . display . license
getField (FieldName "license-file")  = licenseFiles
getField (FieldName "license-files") = licenseFiles
getField (FieldName "copyright")     = (:[]) . copyright
getField (FieldName "maintainer")    = (:[]) . maintainer
getField (FieldName "author")        = (:[]) . author
getField (FieldName "stability")     = (:[]) . stability
getField (FieldName "homepage")      = (:[]) . homepage
getField (FieldName "package-url")   = (:[]) . pkgUrl
getField (FieldName "bug-reports")   = (:[]) . bugReports
getField (FieldName "synopsis")      = (:[]) . synopsis
getField (FieldName "description")   = (:[]) . description
getField (FieldName "category")      = (:[]) . category
getField (FieldName "build-type")    = map display . maybeToList . buildType
-- Library
getField (FieldName "exposed") = maybe [] ((:[]) . display . libExposed) . library
getField (FieldName "exposed-modules") = map display . maybe [] exposedModules . library
getField (FieldName "other-modules")   = map display . maybe [] (otherModules . libBuildInfo) . library
-- Catch-all
getField _ = const []

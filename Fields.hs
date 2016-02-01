-- | Accessing fields from packages.
module Fields
  ( FieldName(..)
  , getField
  ) where

import Data.Maybe (maybeToList)

import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text (Text, display)
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
-- TODO: executables
-- TODO: testSuites
-- TODO: benchmarks
-- TODO: dataFiles
-- TODO: dataDir
-- TODO: extraSrcFiles
-- TODO: extraTmpFiles
-- TODO: extraDocFiles
--
-- The following don't show up in the Cabal User Guide as of
-- 2016-02-01, and so are intentionally omitted for now:
--
-- - library requiredSignatures
-- - library exposedSignatures
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
getField (FieldName "exposed") = libField libExposed
getField (FieldName "exposed-modules") = libListField exposedModules
getField (FieldName "reexported-modules") = libListField reexportedModules
getField (FieldName "build-depends") = libListField (targetBuildDepends . libBuildInfo)
getField (FieldName "other-modules") = libListField (otherModules . libBuildInfo)
getField (FieldName "hs-source-dirs") = libListField' id (hsSourceDirs . libBuildInfo)
getField (FieldName "extensions") = libListField (oldExtensions . libBuildInfo)
getField (FieldName "default-extensions") = libListField (defaultExtensions . libBuildInfo)
getField (FieldName "other-extensions") = libListField (otherExtensions . libBuildInfo)
getField (FieldName "build-tools") = libListField (buildTools . libBuildInfo)
getField (FieldName "buildable") = libField (buildable . libBuildInfo)
getField (FieldName "ghc-options") = libListField' id (concatMap snd . filter ((==GHC) . fst) . options . libBuildInfo)
getField (FieldName "ghc-prof-options") = libListField' id (concatMap snd . filter ((==GHC) . fst) . profOptions . libBuildInfo)
getField (FieldName "ghc-shared-options") = libListField' id (concatMap snd . filter ((==GHC) . fst) . sharedOptions . libBuildInfo)
getField (FieldName "includes") = libListField' id (includes . libBuildInfo)
getField (FieldName "install-includes") = libListField' id (installIncludes . libBuildInfo)
getField (FieldName "include-dirs") = libListField' id (includeDirs . libBuildInfo)
getField (FieldName "c-sources") = libListField' id (cSources . libBuildInfo)
getField (FieldName "js-sources") = libListField' id (jsSources . libBuildInfo)
getField (FieldName "extra-libraries") = libListField' id (extraLibs . libBuildInfo)
getField (FieldName "extra-ghci-libraries") = libListField' id (extraGHCiLibs . libBuildInfo)
getField (FieldName "extra-lib-dirs") = libListField' id (extraLibDirs . libBuildInfo)
getField (FieldName "cc-options") = libListField' id (ccOptions . libBuildInfo)
getField (FieldName "cpp-options") = libListField' id (cppOptions . libBuildInfo)
getField (FieldName "ld-options") = libListField' id (ldOptions . libBuildInfo)
getField (FieldName "pkgconfig-depends") = libListField (pkgconfigDepends . libBuildInfo)
getField (FieldName "frameworks") = libListField' id (frameworks . libBuildInfo)
-- Catch-all
getField _ = const []

-- | Get a value field from a library entry.
libField :: Text a => (Library -> a) -> PackageDescription -> [String]
libField = libField' display

-- | Get a value field from a library entry.
libField' :: (a -> String) -> (Library -> a) -> PackageDescription -> [String]
libField' d f = maybeToList . fmap (d . f) . library

-- | Get a list field from a library entry.
libListField :: Text a => (Library -> [a]) -> PackageDescription -> [String]
libListField = libListField' display

-- | Get a list field from a library entry.
libListField' :: (a -> String) -> (Library -> [a]) -> PackageDescription -> [String]
libListField' d f = map d . maybe [] f . library

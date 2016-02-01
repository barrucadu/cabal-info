-- | Accessing fields from packages.
module Fields
  ( FieldName(..)
  , getField
  ) where

import Data.Maybe (maybeToList)

import Distribution.Compiler (CompilerFlavor(GHC))
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
getField (FieldName "exposed") = maybe [] ((:[]) . display . libExposed) . library
getField (FieldName "exposed-modules") = maybe [] (map display . exposedModules) . library
getField (FieldName "reexported-modules") = maybe [] (map display . reexportedModules) . library
-- Catch-all
getField (FieldName field)
  | field `elem` buildInfoFields = maybe [] (getBuildInfoField field . libBuildInfo) . library
  | otherwise = const []

-- | Get a field from some 'BuildInfo'.
getBuildInfoField :: String -> BuildInfo -> [String]
getBuildInfoField "extra-libraries"      = extraLibs
getBuildInfoField "extra-ghci-libraries" = extraGHCiLibs
getBuildInfoField "extra-lib-dirs"       = extraLibDirs
getBuildInfoField "extensions"         = map display . oldExtensions
getBuildInfoField "default-extensions" = map display . defaultExtensions
getBuildInfoField "other-extensions"   = map display . otherExtensions
getBuildInfoField "ghc-options"        = concatMap snd . filter ((==GHC) . fst) . options
getBuildInfoField "ghc-prof-options"   = concatMap snd . filter ((==GHC) . fst) . profOptions
getBuildInfoField "ghc-shared-options" = concatMap snd . filter ((==GHC) . fst) . sharedOptions
getBuildInfoField "pkgconfig-depends"  = map display . pkgconfigDepends
getBuildInfoField "install-includes"   = installIncludes
getBuildInfoField "hs-source-dirs" = hsSourceDirs
getBuildInfoField "build-depends"  = map display . targetBuildDepends
getBuildInfoField "other-modules"  = map display . otherModules
getBuildInfoField "include-dirs"   = includeDirs
getBuildInfoField "build-tools" = map display . buildTools
getBuildInfoField "cc-options"  = ccOptions
getBuildInfoField "cpp-options" = cppOptions
getBuildInfoField "ld-options"  = ldOptions
getBuildInfoField "c-sources"  = cSources
getBuildInfoField "js-sources" = jsSources
getBuildInfoField "frameworks" = frameworks
getBuildInfoField "buildable"  = (:[]) . display . buildable
getBuildInfoField "includes" = includes

-- | All the fields in a 'BuildInfo'
buildInfoFields :: [String]
buildInfoFields = ["build-depends", "other-modules", "hs-source-dirs", "extensions", "default-extensions", "other-extensions", "build-tools", "buildable", "ghc-options", "ghc-prof-options", "ghc-shared-options", "includes", "install-includes", "include-dirs", "c-sources", "js-sources", "extra-libraries", "extra-ghci-libraries", "extra-lib-dirs", "cc-options", "ld-options", "pkgconfig-depends", "frameworks"]

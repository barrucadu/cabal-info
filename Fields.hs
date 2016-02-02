-- | Accessing fields from packages.
module Fields where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.Maybe (listToMaybe, maybeToList)

import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text (display)
import Distribution.Version

-- | A field name is a string, optionally qualified with a specific
-- executable/test-suite/benchmark.
data FieldName = FieldName (Maybe String) String
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
-- The following don't show up in the Cabal User Guide as of
-- 2016-02-01, and so are intentionally omitted for now:
--
-- - library requiredSignatures
-- - library exposedSignatures
getField :: FieldName -> PackageDescription -> String
-- Special case for the first executable
getField (FieldName Nothing "main-is") = maybe "" (getExecutableField "main-is") . listToMaybe . executables
getField (FieldName Nothing "executable") = maybe "" (getExecutableField "name") . listToMaybe . executables
-- Qualified Fields
getField (FieldName (Just name) field) = \pkg ->
  let exe   = listToMaybe $ filter (\e -> map toLower (exeName  e) == name) (executables pkg)
      test  = listToMaybe $ filter (\t -> map toLower (testName t) == name) (testSuites  pkg)
      bench = listToMaybe $ filter (\b -> map toLower (benchmarkName b) == name) (benchmarks pkg)
  in case (exe, test, bench) of
       (Just e, _, _) -> getExecutableField field e
       (_, Just t, _) -> getTestSuiteField  field t
       (_, _, Just b) -> getBenchmarkField  field b
       _ -> ""
-- Catch-all
getField (FieldName Nothing field)
  | field `elem` packageDescriptionFields = getPackageDescriptionField field

  | field `elem` libraryFields = maybe "" (getLibraryField field) . library

  | field `elem` buildInfoFields = \pkg ->
    let lib = libBuildInfo <$> library pkg
        exe = buildInfo <$> listToMaybe (executables pkg)
    in maybe "" (getBuildInfoField field) (lib <|> exe)

  | otherwise = const ""

-- | Get a field from a 'PackageDescription'.
--
-- TODO: testedWith
-- TODO: sourceRepos
-- TODO: buildDepends
-- TODO: dataFiles
-- TODO: dataDir
-- TODO: extraSrcFiles
-- TODO: extraTmpFiles
-- TODO: extraDocFiles
getPackageDescriptionField :: String -> PackageDescription -> String
getPackageDescriptionField "license-files" = unlines' . licenseFiles
getPackageDescriptionField "license-file" = unlines' . licenseFiles
getPackageDescriptionField "package-url" = pkgUrl
getPackageDescriptionField "bug-reports" = bugReports
getPackageDescriptionField "description" = description
getPackageDescriptionField "maintainer" = maintainer
getPackageDescriptionField "build-type" = unlines' . map display . maybeToList . buildType
getPackageDescriptionField "copyright" = copyright
getPackageDescriptionField "stability" = stability
getPackageDescriptionField "homepage" = homepage
getPackageDescriptionField "synopsis" = synopsis
getPackageDescriptionField "category" = category
getPackageDescriptionField "version" = display . pkgVersion . package
getPackageDescriptionField "license" = display . license
getPackageDescriptionField "author" = author
getPackageDescriptionField "name" = unPackageName . pkgName . package
getPackageDescriptionField _ = const ""

-- | All the fields in a 'PackageDescription'.
packageDescriptionFields :: [String]
packageDescriptionFields = ["name", "version", "license", "license-files", "copyright", "maintainer", "author", "stability", "homepage", "package-url", "bug-reports", "synopsis", "description", "category", "build-type"]

-- | Get a field from a 'Library'.
getLibraryField :: String -> Library -> String
getLibraryField "exposed" = display . libExposed
getLibraryField "exposed-modules" = unlines' . map display . exposedModules
getLibraryField "reexported-modules" = unlines' . map display . reexportedModules
getLibraryField field = getBuildInfoField field . libBuildInfo

-- | All the fields from a 'Library'.
libraryFields :: [String]
libraryFields = ["exposed", "exposed-modules", "reexported-modules"]

-- | Get a field from an 'Executable'.
getExecutableField :: String -> Executable -> String
getExecutableField "name"    = exeName
getExecutableField "main-is" = modulePath
getExecutableField field = getBuildInfoField field . buildInfo

-- | All the fields in an 'Executable'.
executableFields :: [String]
executableFields = ["name", "main-is"]

-- | Get a field from a 'TestSuite'.
getTestSuiteField :: String -> TestSuite -> String
getTestSuiteField "name" = testName
getTestSuiteField "type" = get . testInterface where
  get (TestSuiteExeV10 _ _) = "exitcode-stdio-1.0"
  get (TestSuiteLibV09 _ _) = "detailed-0.9"
  get (TestSuiteUnsupported (TestTypeExe v)) = "exitcode-stdio-" ++ display v
  get (TestSuiteUnsupported (TestTypeLib v)) = "detailed-" ++ display v
  get (TestSuiteUnsupported (TestTypeUnknown s v)) = s ++ "-" ++ display v
getTestSuiteField "main-is" = get . testInterface where
  get (TestSuiteExeV10 _ f) = f
  get _ = ""
getTestSuiteField "test-module" = get . testInterface where
  get (TestSuiteLibV09 _ m) = display m
  get _ = ""
getTestSuiteField "enabled" = display . testEnabled
getTestSuiteField field = getBuildInfoField field . testBuildInfo

-- | All the fields in a 'TestSuite'.
testSuiteFields :: [String]
testSuiteFields = ["name", "type", "main-is", "test-module", "enabled"]

-- | Get a field from a 'Benchmark'.
getBenchmarkField :: String -> Benchmark -> String
getBenchmarkField "name" = benchmarkName
getBenchmarkField "type" = get . benchmarkInterface where
  get (BenchmarkExeV10 _ _) = "exitcode-stdio-1.0"
  get (BenchmarkUnsupported (BenchmarkTypeExe v)) = "exitcode-stdio-" ++ display v
  get (BenchmarkUnsupported (BenchmarkTypeUnknown s v)) = s ++ "-" ++ display v
getBenchmarkField "main-is" = get . benchmarkInterface where
  get (BenchmarkExeV10 _ f) = f
  get _ = ""
getBenchmarkField "enabled" = display . benchmarkEnabled
getBenchmarkField field = getBuildInfoField field . benchmarkBuildInfo

-- | All the fields in a 'Benchmark'.
benchmarkFields :: [String]
benchmarkFields = ["name", "type", "main-is", "enabled"]

-- | Get a field from some 'BuildInfo'.
getBuildInfoField :: String -> BuildInfo -> String
getBuildInfoField field = unlines' . get field where
  get "extra-libraries"      = extraLibs
  get "extra-ghci-libraries" = extraGHCiLibs
  get "extra-lib-dirs"       = extraLibDirs
  get "extensions"         = map display . oldExtensions
  get "default-extensions" = map display . defaultExtensions
  get "other-extensions"   = map display . otherExtensions
  get "ghc-options"        = concatMap snd . filter ((==GHC) . fst) . options
  get "ghc-prof-options"   = concatMap snd . filter ((==GHC) . fst) . profOptions
  get "ghc-shared-options" = concatMap snd . filter ((==GHC) . fst) . sharedOptions
  get "pkgconfig-depends"  = map display . pkgconfigDepends
  get "install-includes"   = installIncludes
  get "hs-source-dirs" = hsSourceDirs
  get "build-depends"  = map display . targetBuildDepends
  get "other-modules"  = map display . otherModules
  get "include-dirs"   = includeDirs
  get "build-tools" = map display . buildTools
  get "cc-options"  = ccOptions
  get "cpp-options" = cppOptions
  get "ld-options"  = ldOptions
  get "c-sources"  = cSources
  get "js-sources" = jsSources
  get "frameworks" = frameworks
  get "buildable"  = (:[]) . display . buildable
  get "includes" = includes
  get _ = const []

-- | All the fields in a 'BuildInfo'
buildInfoFields :: [String]
buildInfoFields = ["build-depends", "other-modules", "hs-source-dirs", "extensions", "default-extensions", "other-extensions", "build-tools", "buildable", "ghc-options", "ghc-prof-options", "ghc-shared-options", "includes", "install-includes", "include-dirs", "c-sources", "js-sources", "extra-libraries", "extra-ghci-libraries", "extra-lib-dirs", "cc-options", "ld-options", "pkgconfig-depends", "frameworks"]

-- | Like 'unlines', but don't include the trailing newline.
unlines' :: [String] -> String
unlines' = init' . unlines where
  init' [] = []
  init' xs = init xs

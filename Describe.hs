-- | Pretty-printing packages.
module Describe where

import Data.List (delete)
import Data.Maybe (maybeToList)
import Distribution.PackageDescription (PackageDescription(..), Library, Executable, TestSuite, Benchmark)

import Fields

-- | A description of a part of a package.
--
-- For executables, testsuites, and benchmarks, the name is not
-- included in the list of fields.
data Description =
    Package [(String, String)] [Description]
  | Library [(String, String)]
  | Executable String [(String, String)]
  | TestSuite String [(String, String)]
  | Benchmark String [(String, String)]
  deriving Show

-- | Pretty-print a package description.
describe :: PackageDescription -> String
describe = show . describePackage

-- | Produce all defined fields in a package.
describePackage :: PackageDescription -> Description
describePackage pkg = Package fields sections where
  fields   = getFields packageDescriptionFields getPackageDescriptionField pkg
  sections = maybeToList lib ++ exes ++ tests ++ benchs
  lib    = describeLibrary    <$> library pkg
  exes   = describeExecutable <$> executables pkg
  tests  = describeTestSuite  <$> testSuites pkg
  benchs = describeBenchmark  <$> benchmarks pkg

-- | Produce all defined fields in a library.
describeLibrary :: Library -> Description
describeLibrary = Library . getFields libraryFields getLibraryField

-- | Produce all defined fields in an executable.
describeExecutable :: Executable -> Description
describeExecutable exe = Executable name fields where
  name   = getExecutableField "name" exe
  fields = getFields (delete "name" executableFields) getExecutableField exe

-- | Produce all defined fields in a test suite.
describeTestSuite :: TestSuite -> Description
describeTestSuite test = TestSuite name fields where
  name   = getTestSuiteField "name" test
  fields = getFields (delete "name" testSuiteFields) getTestSuiteField test

-- | Produce all defined fields in a benchmark.
describeBenchmark :: Benchmark -> Description
describeBenchmark bench = Benchmark name fields where
  name   = getBenchmarkField "name" bench
  fields = getFields (delete "name" benchmarkFields) getBenchmarkField bench

-- | Get all non-empty fields.
getFields :: [String] -> (String -> a -> String) -> a -> [(String, String)]
getFields fields get a = [(f, v) | f <- fields, let v = get f a, not (null v)]

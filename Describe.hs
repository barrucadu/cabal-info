-- | Pretty-printing packages.
module Describe where

import Data.List (elemIndex, delete, intercalate, isInfixOf, isPrefixOf)
import Data.Maybe (fromJust, maybeToList)
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
describe = prettyPrint . describePackage where
  prettyPrint (Package fields rest) = intercalate "\n" $ prettyPrintFields fields : map prettyPrint rest
  prettyPrint (Library fields) = "library\n" ++ indent 2 (prettyPrintFields fields)
  prettyPrint (Executable name fields) = "executable " ++ name ++ "\n" ++ indent 2 (prettyPrintFields fields)
  prettyPrint (TestSuite  name fields) = "test-suite " ++ name ++ "\n" ++ indent 2 (prettyPrintFields fields)
  prettyPrint (Benchmark  name fields) = "benchmark "  ++ name ++ "\n" ++ indent 2 (prettyPrintFields fields)

  prettyPrintFields fs = foldl pp "" $ map (\(f,v) -> (align (maxlen + 2) (f ++ ":"), v)) fs where
    maxlen = maximum $ map (length . fst) fs

    -- Pretty-print a field and value.
    pp sofar (f, v) = sofar ++ f ++ ppv f v ++ "\n"

    -- Pretty-print a value. Has some special cases.
    ppv f v
      -- Special case for description: just want to indent further
      -- lines by 2 spaces, and replace blank lines with an indented
      -- '.'.
      | "description" `isPrefixOf` f = indentS 2 . unlines' . map (\l -> if null l then "." else l) . lines $ v

      -- Special case for non-description multi-line fields: want to
      -- indent non-first-lines to the ":" in the field name, and add
      -- commas.
      | "\n" `isInfixOf` v = indentS (length f - 2) $ mapLines (", " ++) v

      -- Normal case is just the value.
      | otherwise = v

    -- Apply padding at the end of the line to the desired length.
    align n s = s ++ replicate (n - length s) ' '

  -- Indent every line.
  indent n = unlines . map (replicate n ' '++) . lines

  -- Indent every line after the first.
  indentS n = mapLines (replicate n ' '++)

  -- Apply a function to all but the first line.
  mapLines f str = case lines str of
    (x:xs) -> unlines' $ x : map f xs
    [] -> ""

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
describeLibrary = Library . getFields (libraryFields ++ buildInfoFields) getLibraryField

-- | Produce all defined fields in an executable.
describeExecutable :: Executable -> Description
describeExecutable exe = Executable name fields where
  name   = getExecutableField "name" exe
  fields = getFields (delete "name" (executableFields ++ buildInfoFields)) getExecutableField exe

-- | Produce all defined fields in a test suite.
describeTestSuite :: TestSuite -> Description
describeTestSuite test = TestSuite name fields where
  name   = getTestSuiteField "name" test
  fields = getFields (delete "name" (testSuiteFields ++ buildInfoFields)) getTestSuiteField test

-- | Produce all defined fields in a benchmark.
describeBenchmark :: Benchmark -> Description
describeBenchmark bench = Benchmark name fields where
  name   = getBenchmarkField "name" bench
  fields = getFields (delete "name" (benchmarkFields ++ buildInfoFields)) getBenchmarkField bench

-- | Get all non-empty fields.
getFields :: [String] -> (String -> a -> String) -> a -> [(String, String)]
getFields fields get a = [(f, v) | f <- fields, let v = get f a, not (null v)]

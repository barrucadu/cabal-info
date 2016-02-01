-- | Command-line argument handling.
module Args
  ( Args(..)
  , getArgs
  , getDefaultCabalFile
  ) where

import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Options.Applicative
import Distribution.PackageDescription (FlagAssignment, FlagName(..))
import System.Directory (getDirectoryContents)
import System.FilePath (FilePath, takeExtension)

import Fields

data Args = Args
  { cabalFile :: Maybe FilePath
  , flags     :: FlagAssignment
  , field     :: Maybe FieldName
  }
  deriving Show

-- | Parse the command-line arguments.
getArgs :: IO Args
getArgs = do
  defCabal <- getDefaultCabalFile
  args     <- execParser opts

  pure $
    case cabalFile args of
      Just _  -> args
      Nothing -> args { cabalFile = defCabal }

  where
    opts = info (helper <*> argsParser)
      (fullDesc <> progDesc "Print fields from a cabal file")

-- | Try to find a cabal file in the current directory.
getDefaultCabalFile :: IO (Maybe FilePath)
getDefaultCabalFile = listToMaybe . filter ((==".cabal") . takeExtension) <$> getDirectoryContents "."

-------------------------------------------------------------------------------
-- Parsers

argsParser :: Parser Args
argsParser = Args
  <$> optional (strOption
      $  long "cabal-file"
      <> metavar "FILE"
      <> help "The cabal file to use. If unspecified, the first one found in this directory is used instead.")

  <*> flagAssignmentParser

  <*> optional fieldNameParser

flagAssignmentParser :: Parser FlagAssignment
flagAssignmentParser = map go . words <$> strOption (long "flags" <> short 'f' <> metavar "FLAGS" <> help "Force values for the given flags in Cabal conditionals in the .cabal file. E.g. --flags=\"debug -usebytestrings\" forces the flag \"debug\" to true and the flag \"usebytestrings\" to false." <> value "") where

  go ('-':flag) = (FlagName flag, False)
  go flag = (FlagName flag, True)

fieldNameParser :: Parser FieldName
fieldNameParser = FieldName . map toLower <$> argument str (metavar "FIELD")

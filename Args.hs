-- | Command-line argument handling.
module Args
  ( Args(..)
  , getArgs
  , getDefaultCabalFile
  ) where

import Data.Maybe (listToMaybe)
import Options.Applicative
import Distribution.PackageDescription (FlagAssignment, FlagName(..))
import System.Directory (getDirectoryContents)
import System.FilePath (FilePath, takeExtension)

data Args = Args
  { cabalFile :: Maybe FilePath
  , flags     :: FlagAssignment
  , field     :: Maybe String
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

-- | A parser for the arguments.
--
-- usage: cabal-info [--cabal-file FILE] [--flags=FLAGS] [FIELD]
argsParser :: Parser Args
argsParser = Args
  <$> optional (strOption
      $  long "cabal-file"
      <> metavar "FILE"
      <> help "The cabal file to use. If unspecified, the first one found in this directory is used instead.")

  <*> flagAssignmentParser

  <*> optional (argument str $ metavar "FIELD")

-- | Parse a set of flag assignments
flagAssignmentParser :: Parser FlagAssignment
flagAssignmentParser = map go . words <$> strOption (long "flags" <> short 'f' <> metavar "FLAGS" <> help "Force values for the given flags in Cabal conditionals in the .cabal file. E.g. --flags=\"debug -usebytestrings\" forces the flag \"debug\" to true and the flag \"usebytestrings\" to false." <> value "") where

  go ('-':flag) = (FlagName flag, False)
  go flag = (FlagName flag, True)

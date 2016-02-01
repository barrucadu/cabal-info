-- | Command-line argument handling.
module Args
  ( Args(..)
  , getArgs
  , getDefaultCabalFile
  ) where

import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Options.Applicative
import System.Directory (getDirectoryContents)
import System.FilePath (FilePath, takeExtension)

data Args = Args
  { cabalFile :: Maybe FilePath
  , field     :: Maybe String
  }

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
-- usage: cabal-info [--cabal-file FILE] [FIELD]
argsParser :: Parser Args
argsParser = Args
  <$> optional (strOption
      $  long "cabal-file"
      <> metavar "FILE"
      <> help "The cabal file to use. If unspecified, the first one found in this directory is used instead.")

  <*> optional (argument str $ metavar "FIELD")

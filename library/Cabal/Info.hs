{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Get information from cabal files.
module Cabal.Info
  ( -- * Errors
    CabalError(..)
  , prettyPrintErr

  -- * Reading .cabal files
  , findCabalFile
  , findPackageDescription
  , findPackageDescription'
  , findGenericPackageDescription
  , openPackageDescription
  , openPackageDescription'
  , openGenericPackageDescription

  -- * Libraries
  , getLibrary
  , getLibraryModules

  -- * Modules
  , moduleFilePath
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (unless)

import Data.Maybe (fromMaybe, listToMaybe)

import Distribution.Compiler
import Distribution.InstalledPackageInfo (PError(..))
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System

import System.FilePath
import System.Directory (getCurrentDirectory, getDirectoryContents)

-- * Errors

-- | Automatically finding and dealing with a .cabal file failed for
-- some reason.
data CabalError =
    NoCabalFile
  -- ^ A .cabal file could not be found in the current directory or
  -- any of its parents.
  | ParseError FilePath PError
  -- ^ A file with the extension .cabal was found, but could not be
  -- parsed.
  | NoFlagAssignment FilePath
  -- ^ A consistent flag assignment could not be found.
  | NoLibrary FilePath
  -- ^ There is no library section.
  deriving (Eq, Show)

-- | Pretty-print an error.
prettyPrintErr :: CabalError -> String
prettyPrintErr NoCabalFile = "Could not find .cabal file."
prettyPrintErr (ParseError fp err) = "Parse error in " ++ fp ++ ": " ++ show' err ++ "." where
  show' (AmbiguousParse _ l) = "ambiguous parse on line " ++ show l
  show' (NoParse _ l) = "no parse on line " ++ show l
  show' (TabsError l) = "tabbing error on line " ++ show l
  show' (FromString _ (Just l)) = "no parse on line " ++ show l
  show' (FromString _ Nothing) = "no parse"
prettyPrintErr (NoFlagAssignment fp) = "Could not find flag assignment for " ++ fp ++ "."
prettyPrintErr (NoLibrary fp) = "Missing library section in " ++ fp ++ "."

-- * Reading .cabal files

-- | Find the .cabal file.
--
-- If there are .cabal files in the current directory, the first is
-- read. Otherwise, the parent directory is checked. This continues
-- until the filesystem root is reached without finding a .cabal file.
findCabalFile :: IO (Maybe FilePath)
findCabalFile = do
  cwd <- getCurrentDirectory
  listToMaybe <$> findFile ((==".cabal") . takeExtension) (dirs cwd)

  where
    dirs dir = takeWhile (\d -> takeDirectory d /= d) (iterate takeDirectory dir) ++ [takeDrive dir]

    findFile p (d:ds) = (++) <$> (filter p . map (d</>) <$> getDirectoryContents d) <*> findFile p ds
    findFile _ [] = pure []

-- | Find and read the .cabal file, applying the default flags.
findPackageDescription :: IO (Either CabalError (PackageDescription, FilePath))
findPackageDescription = findPackageDescription' [] Nothing Nothing

-- | Find and read the .cabal file, applying the given flags,
-- operating system, and architecture.
findPackageDescription' :: FlagAssignment -> Maybe OS -> Maybe Arch -> IO (Either CabalError (PackageDescription, FilePath))
findPackageDescription' flags os arch = findCabalFile >>=
   maybe (pure $ Left NoCabalFile) (\fp -> fmap (,fp) <$> openPackageDescription' flags os arch fp)

-- | Find and read the .cabal file.
findGenericPackageDescription :: IO (Either CabalError (GenericPackageDescription, FilePath))
findGenericPackageDescription = findCabalFile >>=
  maybe (pure $ Left NoCabalFile) (\fp -> fmap (,fp) <$> openGenericPackageDescription fp)

-- | Open and parse a .cabal file, applying the default flags.
openPackageDescription :: FilePath -> IO (Either CabalError PackageDescription)
openPackageDescription = openPackageDescription' [] Nothing Nothing

-- | Open and parse a .cabal file, and apply the given flags,
-- operating system, and architecture.
openPackageDescription' :: FlagAssignment -> Maybe OS -> Maybe Arch -> FilePath -> IO (Either CabalError PackageDescription)
openPackageDescription' flags os arch fp = openGenericPackageDescription fp <$$> \case
  Right gpkg -> either (const . Left $ NoFlagAssignment fp) (Right . fst) $
    finalizePackageDescription flags (const True) platform compiler [] gpkg
  Left err -> Left err

  where
    platform = Platform (fromMaybe buildArch arch) (fromMaybe buildOS os)
    compiler = unknownCompilerInfo buildCompilerId NoAbiTag

-- | Open and parse a .cabal file.
openGenericPackageDescription :: FilePath -> IO (Either CabalError GenericPackageDescription)
openGenericPackageDescription fp = do
  cabalFile <- readFile fp
  pure $ case parsePackageDescription cabalFile of
    ParseOk _ pkg -> Right pkg
    ParseFailed err -> Left $ ParseError fp err

-- * Libraries

-- | Search for the .cabal file and return its library section.
getLibrary :: IO (Either CabalError Library)
getLibrary = findPackageDescription <$$> \case
  Right (pkgd, fp) -> maybe (Left $ NoLibrary fp) Right $ library pkgd
  Left err -> Left err

-- | Search for the .cabal file and return its exposed library
-- modules, as absolute paths.
getLibraryModules :: IO (Either CabalError [FilePath])
getLibraryModules = findPackageDescription <$$> \case
  Right (pkgd, fp) -> maybe (Left $ NoLibrary fp) (\l -> Right . map (moddir fp l) $ exposedModules l) $ library pkgd
  Left err -> Left err

  where
    moddir fp l m = dropFileName fp </> moduleFilePath (libBuildInfo l) m

-- * Modules

-- | Turn a module name + some build info to a file path taking the
-- hs-source-dirs field into account.
--
-- This path will be relative to the .cabal file.
moduleFilePath :: BuildInfo -> ModuleName -> FilePath
moduleFilePath b m = joinPath ((fromMaybe "" . listToMaybe $ hsSourceDirs b) : components m) <.> "hs"

-- * Utils

-- | Flipped fmap
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap

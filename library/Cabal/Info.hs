{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Get information from cabal files.
module Cabal.Info
  ( -- * Reading .cabal files
    findCabalFile
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
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Verbosity (silent)

import System.FilePath
import System.Directory (getCurrentDirectory, getDirectoryContents)

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
findPackageDescription :: IO (Either String (PackageDescription, FilePath))
findPackageDescription = findPackageDescription' [] Nothing Nothing

-- | Find and read the .cabal file, applying the given flags,
-- operating system, and architecture.
findPackageDescription' :: FlagAssignment -> Maybe OS -> Maybe Arch -> IO (Either String (PackageDescription, FilePath))
findPackageDescription' flags os arch = handleIt $ findCabalFile >>= parseIt where
  parseIt = maybe (pure $ Left "Could not find .cabal file.") (\fp -> fmap (,fp) <$> openPackageDescription' fp flags os arch)

-- | Find and read the .cabal file.
findGenericPackageDescription :: IO (Either String (GenericPackageDescription, FilePath))
findGenericPackageDescription = handleIt $ findCabalFile >>= parseIt where
  parseIt = maybe (pure $ Left "Could not find .cabal file.") (\fp -> fmap (,fp) <$> openGenericPackageDescription fp)

-- | Open and parse a .cabal file, applying the default flags.
openPackageDescription :: FilePath -> IO (Either String PackageDescription)
openPackageDescription fp = openPackageDescription' fp [] Nothing Nothing

-- | Open and parse a .cabal file, and apply the given flags,
-- operating system, and architecture.
openPackageDescription' :: FilePath -> FlagAssignment -> Maybe OS -> Maybe Arch -> IO (Either String PackageDescription)
openPackageDescription' fp flags os arch = openGenericPackageDescription fp <$$> \case
  Right gpkg -> either (const $ Left "Could not find successful flag assignment.") (Right . fst) $
    finalizePackageDescription flags (const True) platform compiler [] gpkg
  Left err -> Left err

  where
    platform = Platform (fromMaybe buildArch arch) (fromMaybe buildOS os)
    compiler = unknownCompilerInfo buildCompilerId NoAbiTag

-- | Open and parse a .cabal file.
openGenericPackageDescription :: FilePath -> IO (Either String GenericPackageDescription)
openGenericPackageDescription fp = handleIt $ Right <$> readPackageDescription silent fp

-- * Libraries

-- | Search for the .cabal file and return its library section.
getLibrary :: IO (Either String Library)
getLibrary = findPackageDescription <$$> \case
  Right (pkgd, _) -> maybe (Left "No library section.") Right $ library pkgd
  Left err -> Left err

-- | Search for the .cabal file and return its exposed library
-- modules, as absolute paths.
getLibraryModules :: IO (Either String [FilePath])
getLibraryModules = findPackageDescription <$$> \case
  Right (pkgd, fp) -> maybe (Left "No library section.") (\l -> Right . map (moddir fp l) $ exposedModules l) $ library pkgd
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

-- | Handle being unable to read a .cabal file.
handleIt :: IO (Either String a) -> IO (Either String a)
handleIt ma = ma `catch'` (const . pure $ Left "Failed to read .cabal file") where
  catch' :: IO a -> (SomeException -> IO a) -> IO a
  catch' = catch

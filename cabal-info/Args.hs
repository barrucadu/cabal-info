-- | Command-line argument handling.
module Args where

import Data.Char (toLower)
import Options.Applicative
import Distribution.PackageDescription (FlagAssignment, FlagName(..))
import Distribution.System (Arch(..), OS(..))
import System.FilePath (FilePath)

import Fields

data Args = Args
  { cabalFile :: Maybe FilePath
  , flags     :: FlagAssignment
  , theArch   :: Maybe Arch
  , theOS     :: Maybe OS
  , field     :: Maybe FieldName
  }
  deriving Show

-- | Parse the command-line arguments.
getArgs :: IO Args
getArgs = execParser opts where
  opts = info (helper <*> argsParser)
    (fullDesc <> progDesc "Print fields from a cabal file.")

-------------------------------------------------------------------------------
-- Parsers

argsParser :: Parser Args
argsParser = Args
  <$> optional (strOption
      $  long "cabal-file"
      <> metavar "FILE"
      <> help "The cabal file to use. If unspecified, the first one found in this directory is used instead.")

  <*> flagAssignmentParser

  <*> optional archParser

  <*> optional osParser

  <*> optional fieldNameParser

flagAssignmentParser :: Parser FlagAssignment
flagAssignmentParser = map go . words <$> strOption (long "flags" <> short 'f' <> metavar "FLAGS" <> help "Force values for the given flags in Cabal conditionals in the .cabal file. E.g. --flags=\"debug -usebytestrings\" forces the flag \"debug\" to true and the flag \"usebytestrings\" to false." <> value "") where

  go ('-':flag) = (FlagName flag, False)
  go flag = (FlagName flag, True)

osParser :: Parser OS
osParser = go . map toLower <$> strOption (long "os" <> short 'o' <> metavar "OS" <> help "The operating system to use when expanding conditionals. Allowed values are (case insensitive): linux, windows, osx, freebsd, openbsd, netbsd, dragonfly, solaris, aix, hpux, iric, halvm, ios, ghcjs.") where
  go "linux" = Linux
  go "windows" = Windows
  go "osx" = OSX
  go "freebsd" = FreeBSD
  go "netbsd" = NetBSD
  go "dragonfly" = DragonFly
  go "solaris" = Solaris
  go "aix" = AIX
  go "hpux" = HPUX
  go "irix" = IRIX
  go "halvm" = HaLVM
  go "ios" = IOS
  go "ghcjs" = Ghcjs
  go os = OtherOS os

archParser :: Parser Arch
archParser = go . map toLower <$> strOption (long "arch" <> short 'a' <> metavar "ARCH" <> help "The architecture to use when expanding conditionals. Allowed values are (case insensitive): i386, x86_64, ppc, ppc64, sparc, arm, mips, sh, ia64, s390, alpha, hppa, rs6000, m68k, vax, javascript.") where
  go "i386" = I386
  go "x86_64" = X86_64
  go "ppc" = PPC
  go "ppc64" = PPC64
  go "sparc" = Sparc
  go "arm" = Arm
  go "mips" = Mips
  go "sh" = SH
  go "ia64" = IA64
  go "s390" = S390
  go "alpha" = Alpha
  go "hppa" = Hppa
  go "rs6000" = Rs6000
  go "m68k" = M68k
  go "vax" = Vax
  go "javascript" = JavaScript
  go arch = OtherArch arch

fieldNameParser :: Parser FieldName
fieldNameParser = go <$> argument str (metavar "FIELD" <> help "This is in the format [section:]field, where the section can be the name of a source repository, executable, test suite, or benchmark. If no field is given, then the file is pretty-printed, with any flags applied.") where
  go fname = case break (==':') fname of
    (name, ':':field) -> FieldName (Just $ map toLower name) (map toLower field)
    (field, []) -> FieldName Nothing (map toLower field)

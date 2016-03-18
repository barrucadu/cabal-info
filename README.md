cabal-info
==========

Have you ever needed to get information from a cabal file in a shell
script? Now you can! `cabal-info` exposes a simple command-line
interface to the cabal file format.

There is also a library interface, to solve tasks based on searching
for a .cabal file and then doing something with it. For example, to
find a .cabal file, get the path to every module it exposes, and run
doctest on those:

```haskell
import Test.Doctest (doctest)
import Cabal.Info (getLibraryModules)

main :: IO ()
main = getLibraryModules >>= doctest . either (const []) id
```

The library documentation of the latest developmental version is
[available online][docs].

Usage
-----

```
Usage: cabal-info [--cabal-file FILE] [-f|--flags FLAGS] [-a|--arch ARCH]
                  [-o|--os OS] [FIELD]
  Print fields from a cabal file.

Available options:
  -h,--help                Show this help text
  --cabal-file FILE        The cabal file to use. If unspecified, the first one
                           found in this directory is used instead.
  -f,--flags FLAGS         Force values for the given flags in Cabal
                           conditionals in the .cabal file. E.g. --flags="debug
                           -usebytestrings" forces the flag "debug" to true and
                           the flag "usebytestrings" to false.
  -a,--arch ARCH           The architecture to use when expanding conditionals.
                           Allowed values are (case insensitive): i386, x86_64,
                           ppc, ppc64, sparc, arm, mips, sh, ia64, s390, alpha,
                           hppa, rs6000, m68k, vax, javascript.
  -o,--os OS               The operating system to use when expanding
                           conditionals. Allowed values are (case insensitive):
                           linux, windows, osx, freebsd, openbsd, netbsd,
                           dragonfly, solaris, aix, hpux, iric, halvm, ios,
                           ghcjs.
  FIELD                    This is in the format [section:]field, where the
                           section can be the name of a source repository,
                           executable, test suite, or benchmark. If no field is
                           given, then the file is pretty-printed, with any
                           flags applied.
```

Examples
--------

Here's a simple cabal file:

```
name:           HUnit
version:        1.1.1
synopsis:       A unit testing framework for Haskell
homepage:       http://hunit.sourceforge.net/
category:       Testing
author:         Dean Herington
license:        BSD3
license-file:   LICENSE
cabal-version:  >= 1.10
build-type:     Simple

library
  build-depends:      base >= 2 && < 4
  exposed-modules:    Test.HUnit.Base, Test.HUnit.Lang,
                      Test.HUnit.Terminal, Test.HUnit.Text, Test.HUnit
  default-extensions: CPP
```

Here are some simple queries:

```
$ cabal-info name
HUnit

$ cabal-info build-depends
base >=2 && <4
```

Multi-value fields are displayed with each value on a new line.

```
$ cabal-info exposed-modules
Test.HUnit.Base
Test.HUnit.Lang
Test.HUnit.Terminal
Test.HUnit.Text
Test.HUnit
```

Now let's look at a more complex example:

```
name:            TestPackage
version:         0.0
synopsis:        Package with library and two programs
license:         BSD3
author:          Angela Author
build-type:      Simple
cabal-version:   >= 1.2

library
  build-depends:   HUnit
  exposed-modules: A, B, C

executable program1
  main-is:         Main.hs
  hs-source-dirs:  prog1
  other-modules:   A, B

executable program2
  main-is:         Main.hs
  hs-source-dirs:  prog2
  other-modules:   A, C, Utils
```

We can use the name of the executable to extract information from
each:

```
$ cabal-info program1:hs-source-dirs
prog1

$ cabal-info program2:hs-source-dirs
prog2
```

There are a few special cases:

- "flag", "executable", "testsuite", "benchmark", and "repository"
  refer to the name of the first flag/executable/etc.
- "flags", "executables", "testsuites", "benchmarks", and
  "repositories" refer to list of names of the flags/executables/etc.
- "main-is" refers to the main-is field of the first executable.
- "upstream" refers to the location of the head source repository.

For example:

```
$ cabal-info main-is
Main.hs

$ cabal-info executable
program1
```

We can also deal with flags:

```
Name: Test1
Version: 0.0.1
Cabal-Version: >= 1.2
License: BSD3
Author:  Jane Doe
Synopsis: Test package to test configurations
Category: Example

Flag Debug
  Description: Enable debug support
  Default:     False

Flag WebFrontend
  Description: Include API for web frontend.
  -- Cabal checks if the configuration is possible, first
  -- with this flag set to True and if not it tries with False

Library
  Build-Depends:   base
  Exposed-Modules: Testing.Test1
  Extensions:      CPP

  if flag(debug)
    GHC-Options: -DDEBUG
    if !os(windows)
      CC-Options: "-DDEBUG"
    else
      CC-Options: "-DNDEBUG"

  if flag(webfrontend)
    Build-Depends: cgi > 0.42
    Other-Modules: Testing.WebStuff

Executable test1
  Main-is: T1.hs
  Other-Modules: Testing.Test1
  Build-Depends: base

  if flag(debug)
    CC-Options: "-DDEBUG"
    GHC-Options: -DDEBUG
```

The "debug" flag defaults to false, and "webfrontend" to true:

```
$ cabal-info test1:cc-options
 
$ cabal-info -fdebug test1:cc-options
-DDEBUG

$ cabal-info build-depends
base -any && -any
cgi >0.42

$ cabal-info -f-webfrontend build-depends
base -any && -any
```

Finally, we can pretty-print an entire cabal file, with flags
applied. This is useful for seeing the effects of different
combinations of flags:

```
$ cabal-info
name:          Test1
version:       0.0.1
build-depends: base -any && -any
             , cgi >0.42
license:       BSD3
author:        Jane Doe
synopsis:      Test package to test configurations
category:      Example

library
  exposed:         True
  exposed-modules: Testing.Test1
  build-depends:   base -any
                 , cgi >0.42
  other-modules:   Testing.WebStuff
  hs-source-dirs:  .
  extensions:      CPP
  buildable:       True

executable test1
  main-is:        T1.hs
  build-depends:  base -any
  other-modules:  Testing.Test1
  hs-source-dirs: .
  buildable:      True


$ cabal-info -f"debug -webfrontend"
name:          Test1
version:       0.0.1
build-depends: base -any && -any
             , cgi >0.42
license:       BSD3
author:        Jane Doe
synopsis:      Test package to test configurations
category:      Example

library
  exposed:         True
  exposed-modules: Testing.Test1
  build-depends:   base -any
                 , cgi >0.42
  other-modules:   Testing.WebStuff
  hs-source-dirs:  .
  extensions:      CPP
  buildable:       True
  ghc-options:     -DDEBUG
  cc-options:      -DDEBUG

executable test1
  main-is:        T1.hs
  build-depends:  base -any
  other-modules:  Testing.Test1
  hs-source-dirs: .
  buildable:      True
  ghc-options:    -DDEBUG
  cc-options:     -DDEBUG
```

Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).

[docs]: https://docs.barrucadu.co.uk/cabal-info

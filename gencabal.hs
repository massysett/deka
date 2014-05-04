module Main where

import qualified Distribution.PackageDescription as D
import qualified Distribution.PackageDescription.PrettyPrint as D
import qualified Distribution.Package as D
import qualified Distribution.License as D
import qualified Distribution.Compiler as D
import qualified Distribution.Version as D
import qualified Distribution.ModuleName as D
import qualified Language.Haskell.Extension as D

name :: D.PackageName
name = D.PackageName "deka"

pkgVersion :: D.Version
pkgVersion = D.Version [0,6,0,0] []

testedWith :: [(D.CompilerFlavor, D.VersionRange)]
testedWith =
  [ (D.GHC, D.thisVersion (D.Version [7,4,2] []))
  , (D.GHC, D.thisVersion (D.Version [7,6,3] []))
  , (D.GHC, D.thisVersion (D.Version [7,8,2] []))
  ]

extraSrc :: [FilePath]
extraSrc =
  [ "README.md"
  , "ChangeLog"
  , "current-versions.txt"
  , "minimum-versions.txt"
  , "configure"
  , "decnumber/src/decNumberLocal.h.in"
  ]

libraryModules :: [D.ModuleName]
libraryModules = map (D.fromString . ("Deka." ++))
  [ "Class"
  , "Context"
  , "DecNum"
  , "Decoded"
  , "Docs"
  , "Docs.Examples"
  , "Fixed"
  , "Fixed.Single"
  , "Fixed.Double"
  , "Fixed.Quad"
  ]

internalModules :: [D.ModuleName]
internalModules
  = map D.fromString $
  "Deka.Internal"
  : map ("Deka.Internal." ++)
  [ "Class"
  , "Context"
  , "DecNum.CtxFree"
  , "DecNum.Ctx"
  , "DecNum.DecNum"
  , "DecNum.Util"
  , "Decnumber.Context"
  , "Decnumber.Decimal32"
  , "Decnumber.Decimal64"
  , "Decnumber.Decimal128"
  , "Decnumber.DecNumber"
  , "Decnumber.DecNumberLocal"
  , "Decnumber.DecDouble"
  , "Decnumber.DecSingle"
  , "Decnumber.DecQuad"
  , "Single.CtxFree"
  , "Single.Ctx"
  , "Single.Decoding"
  , "Single.Single"
  , "Double.CtxFree"
  , "Double.Ctx"
  , "Double.Decoding"
  , "Double.Double"
  , "Quad.CtxFree"
  , "Quad.Ctx"
  , "Quad.Decoding"
  , "Quad.Quad"
  , "Unsafe"
  ]

-- As of GHC 7.8, the ordering of the cSources matters.  See
--
-- https://ghc.haskell.org/trac/ghc/ticket/9074#ticket

-- | C sources.  Do not include:
--
-- * decBasic.c - this is included by decSingle.c, decQuad.c,
-- decDouble.c
--
-- * decCommon.c - this is included by dec{Single,Double,Quad}.c
--
-- * decPacked.c - auxiliary, not needed
cSources :: [FilePath]
cSources = map (\s -> "decnumber/src/" ++ s ++ ".c")
  [ "decContext"
  , "decNumberMacros"
  , "decNumber"
  , "decimal64"
  , "decimal32"
  , "decimal128"
  , "decQuad"
  , "decDouble"
  , "decSingle"
  ]

-- | C headers.  Do not include:
--
-- * decPacked.h - auxiliary, not needed
cHeaders :: [FilePath]
cHeaders = map (\s -> "decnumber/src/" ++ s ++ ".h")
  [ "decContext"
  , "decDouble"
  , "decDPD"
  , "decimal128"
  , "decimal32"
  , "decimal64"
  , "decNumber"
  , "decNumberLocal"
  , "decNumberMacros"
  , "decQuad"
  , "decSingle"
  ]


packageDescription :: D.PackageDescription
packageDescription = D.emptyPackageDescription
  { D.package = D.PackageIdentifier name pkgVersion
  , D.license = D.BSD3
  , D.licenseFile = "LICENSE"
  , D.copyright = "2014 Omari Norman"
  , D.maintainer = "omari@smileystation.com"
  , D.author = "Omari Norman"
  , D.stability = "Experimental"
  , D.testedWith = testedWith
  , D.homepage = "http://www.github.com/massysett/deka"
  , D.pkgUrl = ""
  , D.bugReports = "omari@smileystation.com"
  , D.sourceRepos =
    [ D.SourceRepo
        { D.repoKind = D.RepoHead
        , D.repoType = Just D.Git
        , D.repoLocation = Just "https://github.com/massysett/deka.git"
        , D.repoModule = Nothing
        , D.repoBranch = Just "master"
        , D.repoTag = Nothing
        , D.repoSubdir = Nothing
        }
      ]

  , D.synopsis = "Decimal floating point arithmetic"
  , D.description = unlines
    [ "deka provides decimal floating point arithmetic.  It"
    , "is based on the decNumber C library, which is available"
    , "at"
    , ""
    , "<http://speleotrove.com/decimal/decnumber.html>"
    , ""
    , "decNumber, in turn, implements the General Decimal Arithmetic"
    , "Specification, which is available at"
    , ""
    , "<http://speleotrove.com/decimal/>"
    , ""
    , "For more on deka, please see the Github home page at"
    , ""
    , "<https://github.com/massysett/deka>"
    ]
  , D.category = "Math"
  , D.specVersionRaw = Right (D.orLaterVersion (D.Version [1, 10] []))
  , D.buildType = Just D.Configure

  -- Do not supply "library", "executabltes", "testSuites", or
  -- "benchmarks" as these are supplied in the tree, not here.

  , D.extraSrcFiles = extraSrc

  }

flags :: [D.Flag]
flags =
  [ D.MkFlag
      { D.flagName = D.FlagName "testexe"
      , D.flagDescription = "build test executables"
      , D.flagDefault = False
      , D.flagManual = False
      }
  ]

ghcOptions :: [(D.CompilerFlavor, [String])]
ghcOptions = [(D.GHC, ["-Wall"])]

libBuildInfo :: D.BuildInfo
libBuildInfo = D.emptyBuildInfo
  { D.buildable = True
  , D.cSources = cSources
  , D.hsSourceDirs = ["lib"]
  , D.otherModules = internalModules
  , D.defaultLanguage = Just D.Haskell2010
  , D.includeDirs = ["decnumber/src"]
  , D.includes = cHeaders
  , D.options = ghcOptions
  -- Do not put build dependencies in targetBuildDpends - they go
  -- in the condTreeConstraints, which is in libDeps
  }

between
  :: String
  -- ^ Package name
  -> [Int]
  -- ^ Lower bound
  -> [Int]
  -- ^ Upper bound
  -> D.Dependency
between n l h = D.Dependency (D.PackageName n)
  ((D.orLaterVersion (D.Version l []))
  `D.intersectVersionRanges` (D.earlierVersion (D.Version h [])))

libDeps :: [D.Dependency]
libDeps =
  [ between "base"          [4,5,0,0] [4,8]
  , between "bytestring"    [0,9,2,1] [0,11]
  ]

library :: D.CondTree D.ConfVar [D.Dependency] D.Library
library = D.CondNode
  { D.condTreeData = D.Library
      { D.exposedModules = libraryModules
      , D.libExposed = True
      , D.libBuildInfo = libBuildInfo
      }
  , D.condTreeConstraints = libDeps
  , D.condTreeComponents = []
  }

-- Leave exeName blank - this is probably if you want the executable
-- to have a name different than that specified in the "executables"
-- list

parseTestFile :: D.CondTree D.ConfVar [D.Dependency] D.Executable
parseTestFile = D.CondNode
  { D.condTreeData = D.Executable
      { D.exeName = ""
      , D.modulePath = "testParse.hs"
      , D.buildInfo = D.emptyBuildInfo
          { D.defaultLanguage = Just D.Haskell2010
          , D.hsSourceDirs = [ "tests" ]
          , D.options = ghcOptions
          }
      }

  , D.condTreeConstraints = libDeps
  , D.condTreeComponents =
      [ ( D.CNot (D.Var (D.Flag (D.FlagName "testexe")))
          , D.CondNode
            { D.condTreeData = D.Executable
                { D.exeName = ""
                , D.modulePath = ""
                , D.buildInfo = D.emptyBuildInfo { D.buildable = False }
                }
            , D.condTreeConstraints = []
            , D.condTreeComponents = []
            }
          , Nothing
        )
      ]
    }

tastyTest :: D.CondTree D.ConfVar [D.Dependency] D.TestSuite
tastyTest = D.CondNode
  { D.condTreeData = D.TestSuite
      { D.testName = ""
      , D.testInterface = D.TestSuiteExeV10
          (Version [1,0] []) "tasty-test.hs"
      , D.testBuildInfo = D.emptyBuildInfo
          { D.buildable = True
          , D.cSources = cSources
          , D.hsSourceDirs = ["tests", "lib"]
          , D.otherModules = libraryModules ++ internalModules
              ++ testModules
          , D.defaultLanguage = Just D.Haskell2010
          , D.includeDirs = [ "decnumber/src" ]
          , D.includes = cHeaders
          , D.options = ghcOptions
          }

      -- Why is testEnabled False? I am guessing it's because the
      -- TestSuite is part of the PackageDescription after
      -- conditionals are resolved.
      , D.testEnabled = False
      }
  , D.condTreeConstraints = libDeps ++ testDeps
  }



executables :: [(String, D.CondTree D.ConfVar [D.Dependency] D.Executable)]
executables = [("parseTestFile", parseTestFile)]

testSuites :: [(String, D.CondTree D.ConfVar [D.Dependency] D.TestSuite)]
testSuites = [("tasty-test", tastyTest)]

benchmarks :: [(String, D.CondTree D.ConfVar [D.Dependency] D.Benchmark)]
benchmarks = []

--
-- No configuration variables below this line
--

genericDescription :: D.GenericPackageDescription
genericDescription = D.GenericPackageDescription
  { D.packageDescription = packageDescription
  , D.genPackageFlags = flags
  , D.condLibrary = Just library
  , D.condExecutables = executables
  , D.condTestSuites = testSuites
  , D.condBenchmarks = benchmarks
  }

main :: IO ()
main = putStrLn . D.showGenericPackageDescription $ genericDescription

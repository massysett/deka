-- Generate Cabal file using Cartel.
-- Written for Cartel version 0.10.0.2.
module Main where

import qualified CabalCommon as CC
import qualified Cartel as C

quickpull :: C.Package
quickpull = C.closedOpen "quickpull" [0,2,0,0] [0,3]

properties :: C.Properties
properties = CC.properties
  { C.prName = "deka-tests"
  , C.prSynopsis = "Tests for deka, decimal floating point arithmetic"
  , C.prDescription = CC.description ++
    [ "This package contains only tests, so that other packages"
    , "may also use the tests."
    ]
  }

testDeps :: [C.Package]
testDeps = CC.buildDeps ++
  [ C.exactly "deka" CC.versionInts
  , C.closedOpen "QuickCheck" [2,7,3] [2,8]
  ]

library
  :: [String]
  -- ^ Exposed modules
  -> C.Library
library ex = C.Library
  [ C.LibExposedModules ex
  , C.hsSourceDirs ["lib"]
  , C.buildDepends testDeps
  , C.ghcOptions CC.ghcOptions
  , C.defaultLanguage C.Haskell2010
  ]

native
  :: [String]
  -- ^ Native test modules
  -> [String]
  -- ^ Library modules
  -> C.TestSuite
native ms ls = C.TestSuite "deka-native" $
  [ C.buildDepends $ testDeps ++ [quickpull]
  , C.TestType C.ExitcodeStdio
  , C.TestMainIs "deka-native.hs"
  , C.hsSourceDirs ["native", "lib"]
  , C.otherModules $ ms ++ ls
  , C.defaultLanguage C.Haskell2010
  ]

cabal
  :: [String]
  -- ^ Exposed library modules
  -> [String]
  -- ^ Native modules
  -> C.Cabal
cabal ex nt = C.empty
  { C.cProperties = properties
  , C.cRepositories = [CC.repo]
  , C.cLibrary = Just $ library ex
  , C.cTestSuites = [native ex nt]
  }

main :: IO ()
main = do
  ex <- C.modules "lib"
  nt <- C.modules "native"
  C.render "genCabal.hs" $ cabal ex nt

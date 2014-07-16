module CabalCommon where

import qualified Cartel as C

versionInts :: [Int]
versionInts = [0,6,0,2]

description :: [String]
description =
  [ "deka provides decimal floating point arithmetic.  It is based on"
  , "mpdecimal, the C library used to provide support for the Decimal"
  , "module in Python 3."
  , ""
  , "You will need to install mpdecimal to use deka; otherwise your"
  , "executables will not link.  It is available at"
  , ""
  , " <http://www.bytereef.org/mpdecimal/>"
  , ""
  , "mpdecimal has also been packaged for some Linux distributions,"
  , "such as Debian (libmpdec-dev - available in Jessie and later) and"
  , "Arch (mpdecimal)."
  , ""
  , "mpdecimal, in turn, implements the General Decimal Arithmetic"
  , "Specification, which is available at"
  , ""
  , "<http://speleotrove.com/decimal/>"
  , ""
  , "For more on deka, please see the Github home page at"
  , ""
  , "<https://github.com/massysett/deka>"
  , ""
  ]

properties :: C.Properties
properties = C.empty
  { C.prVersion = C.Version versionInts
  , C.prBuildType = C.Simple
  , C.prLicense = C.BSD3
  , C.prLicenseFile = "LICENSE"
  , C.prCopyright = "Copyright 2014 Omari Norman"
  , C.prAuthor = "Omari Norman"
  , C.prMaintainer = "Omari Norman, omari@smileystation.com"
  , C.prStability = "Experimental"
  , C.prHomepage = "https://github.com/massysett/deka"
  , C.prBugReports = "https://github.com/massysett/deka/issues"
  , C.prCategory = "Math"
  , C.prTestedWith =
    let f v = (C.GHC, C.eq v)
    in map f [[7,4,1], [7,6,3], [7,8,3]]
  , C.prExtraSourceFiles =
    [ "README.md"
    , "ChangeLog"
    , "current-versions.txt"
    , "minimum-versions.txt"
    ]
  }

buildDeps :: [C.Package]
buildDeps =
  [ C.closedOpen "base" [4,5,0,0] [4,8]
  , C.closedOpen "bytestring" [0,9,2,1] [0,11]
  ]

parsec :: C.Package
parsec = C.closedOpen "parsec" [3,1,2] [3,2]

transformers :: C.Package
transformers = C.closedOpen "transformers" [0,3,0,0] [0,5]

ghcOptions :: [String]
ghcOptions = ["-Wall"]

repo :: C.Repository
repo = C.empty
  { C.repoVcs = C.Git
  , C.repoKind = C.Head
  , C.repoLocation = "https://github.com/massysett/deka.git"
  }

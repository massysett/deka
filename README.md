# deka

deka provides correctly rounded decimal arithmetic for Haskell.

The core of deka is a binding to the C library decNumber. As the
author of deka, I have no association with the author of decNumber,
and any errors in this library are mine and should be reported to
omari@smileystation.com or to the Github tracker at

http://www.github.com/massysett/deka

deka uses the decQuad functions in decNumber.  This means that deka
is limited to 34 digits of precision.  Because 1 quadrillion (that
is, one thousand trillion) has only 16 digits of precision, I figure
that 34 should be sufficient for many uses.  Also, you are limited
to exponents no smaller than -6176 and no greater than 6111.  deka
will notify you if you perform calculations that must be rounded in
order to fit within the 34 digits of precision or within the size
limits for the exponent.

You will want to understand decNumber and the General Decimal
Arithmetic Specification in order to fully understand deka.  The
specification is at

http://speleotrove.com/decimal/decarith.html

and decNumber is at

http://speleotrove.com/decimal/decnumber.html

and more about decimal arithmetic generally at

http://speleotrove.com/decimal/

## Dependencies

The main deka library depends only on `base` and `bytestring`, so it
shouldn't be difficult to build.  The
tests use [tasty](http://documentup.com/feuerbach/tasty) and
[QuickCheck](http://hackage.haskell.org/package/QuickCheck).  The
decNumber C library is bundled in; GHC will build it and link it for
you when you install deka.

## More documentation

Much more documentation is available in the Haddock comments in the
source files.  There is also a file of examples to get you started.
It has copious comments.  It is written in literate Haskell, so the
compiler keeps me honest with the example code.  Unfortunately
Haddock does not play very nice with literate Haskell.  However, the
file is easy to view on Github:

[Examples](lib/Data/Deka/Docs/Examples.lhs)

## License

deka is licensed under the BSD license, see the LICENSE file.

## Build status

[![Build Status](https://travis-ci.org/massysett/deka.png?branch=master)](https://travis-ci.org/massysett/deka)


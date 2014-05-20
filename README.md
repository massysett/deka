# deka

deka provides correctly rounded decimal arithmetic for Haskell.

The core of deka is a binding to the C library mpdecimal.  You need
to install mpdecimal; otherwise, your executables will not link.
mpdecimal is available here:

http://www.bytereef.org/mpdecimal/index.html

mpdecimal has also been packaged for some Linux distributions, such
as Debian (libmpdec-dev) and Arch (mpdecimal).  deka has been tested
with mpdecimal version 2.4.0.

As the author of deka, I have no association with the author of
mpdecimal, and any errors in this library are mine and should be
reported to omari@smileystation.com or to the Github tracker at

http://www.github.com/massysett/deka

You will want to understand the General Decimal Arithmetic
Specification in order to fully understand deka.  The specification
is at

http://speleotrove.com/decimal/decarith.html

and more about decimal arithmetic generally at

http://speleotrove.com/decimal/

## Dependencies

The main deka library depends only on `base`, `bytestring`, and
`parsec`, so it shouldn't be difficult to build.  The tests use
[tasty](http://documentup.com/feuerbach/tasty) and
[QuickCheck](http://hackage.haskell.org/package/QuickCheck).

## Test status

deka is tested using the tests available on the General Decimal
Arithmetic website:

http://speleotrove.com/decimal/dectest.html

Some of these tests currently fail.  The failures are in edge cases
that should not affect most usage.  Diagnosing these failures is on
the TODO list.

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


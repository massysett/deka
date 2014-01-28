Examples for the Deka library
=============================

For very simple arithmetic, just import Data.Deka.  It contains a
Deka type, which is an instance of Num.

For work that goes beyond very simple arithmetic, you will
typically import Data.Deka.Pure.  This allows you to run all the
code in pure functions.  "Under the covers" things happen in the
IO monad; however, it is safe to do this work in pure code
because there are no visible side effects.  However, all the Safe
code (that is, Safe in a Safe Haskell sense) is included in
Data.Deka.IO, so you can import that if you wish.

Usually you will want to perform a qualified import, because
Data.Deka.Pure exports a lot of functions that clash with Prelude
names.

> module Data.Deka.Examples where

> import Data.Deka
> import qualified Data.Deka.Pure as P

We need Char8 ByteStrings when working with the Pure and IO modules:

> import qualified Data.ByteString.Char8 as BS8

> examples :: IO ()
> examples = do {

Why is decimal arithmetic important?  The webpages here discuss the
issue at great length:

http://speleotrove.com/decimal/

But in a nutshell, the floats that are built in to nearly every
computer language, including Haskell, are approximate.  That's OK
for many purposes.  It's not OK if you need exact results, such as
for financial purposes.

For example, on my machine this will not output 0.3 but instead will
output 0.3 plus a small fraction:

> print $ 0.1 + 0.1 + (0.1 :: Double);

This sort of imprecision adds up quickly and makes your life as a
programmer harder in many ways.  It also produces results that are
simply incorrect if you needed an exact answer.

For simple arithmetic like this, Deka provides the Deka type.  It is
an instance of Num.  Results with the Deka type are never, ever
rounded.  You are limited to 34 digits of precision.  If you need
more than 34 digits of precision, you can afford to pay someone to
develop your own library :) For example, these numbers all have 5
digits of precision:

12345
123.45
0.12345
0.00012345

All numbers in Deka are stored as a "coefficient" and an "exponent".
The coefficient is an integer, and the exponent is an
integer that may be negative, zero, or positive.  Here, the
coefficient is always 12345, but the exponent varies:

Number               Exponent
12345                0
123.45               -2
0.12345              -5
.00012345            -8

Some numbers can only accurately be written down using scientific
notation if we want to reflect how many digits are in the
coefficient.  We can do this with E notation, where the coefficient
is followed by the exponent.  To get the original number, if the
coefficient is c and the exponent is e, do

c * 10 ^ e

So, for example, you can say that "12345e0" and "1234500e-2" are the
same number, but they have different coefficients.

For more about decimal arithmetic, you will really want to read

http://speleotrove.com/decimal/decarith.html

It's written in a very clear style.

OK, so back to Deka.  We said that "print $ 0.1 + 0.1 + 0.1" yields
an inaccurate result.  How to do it with Deka?

First we have to create a Deka. Deka is not an instance of Read.
However you can use strToDeka, which has the type

strToDeka :: String -> Either String Deka

You get a Left with an error message if your Deka could not be
created; otherwise, you get a Right with the Deka.  The input string
can be in regular or scientific notation.

The following function makes it easier to use strToDeka
interactively:

crashy :: Either String a -> a

It applies the "error" function to the error message if there is
one; otherwise you get the result.

So, the following snippet will not give you incorrectly rounded
results:

> let oneTenth = crashy . strToDeka $ "0.1"
> in print $ oneTenth + oneTenth + oneTenth;

Deka is not an instance of other numeric typeclasses, such as
Real or Fractional.  That's because Deka never ever rounds, no
matter what.  For Deka to be a member of Fractional, it would need
to implement division, and division without rounding can't do very
much.

Sometimes it will be impossible for Deka to do its math without
rounding.  In that case, the functions in the Deka module will apply
"error" and quit.  That way you are assured that if you have a
result, it is not rounded.


More flexibility with the Data.Deka.Pure module
===============================================

Though the Deka type provides you with some flexibility--and it's
easy to use because it's an instance of Num--sometimes you need more
flexibility.  If you want to perform division, for example, Deka is
no good.  For more flexibility, but more cumbersome use, turn to the
Data.Deka.Pure module.

The main type of the Pure module is called Quad, after decQuad in
the decNumber library.  It exposes the full power of the decNumber
library.  The disadvantage is that all computations must be
performed in the Env monad.  This monad carries the state that
decNumber needs to do its work.  It provides you with a lot of
information about any errors that have occurred during computations.

If you are getting into the Pure module, you really need to read the
decimal arithmetic specification at

http://speleotrove.com/decimal/decarith.html

Following is an example of how you would add one tenth using the
Quad type:

> BS8.putStrLn . P.evalEnv $ do
>   oneTenth <- P.fromString . BS8.pack $ "0.1"
>   r1 <- P.add oneTenth oneTenth
>   r2 <- P.add r1 oneTenth
>   P.toString r2; 
> ;

As you can see this is much more cumbersome.  But it does give you
the full power of decNumber.

Rounding
--------

One reason to use the Pure module is because you want greater
control over rounding.  There are many varieties of rounding
available, which you can set.  This can be useful with division, for
example, where you will not get exact results.  All results are
computed to 34 digits of precision.

> let tenThirds = P.evalEnv $ do
>         ten <- P.fromString . BS8.pack $ "10"
>         three <- P.fromString . BS8.pack $ "6"
>         P.divide ten three
>
> in do {

> putStrLn "This is the result of 10 / 6:";
> BS8.putStrLn . P.evalEnv . P.toString $ tenThirds;

Perhaps you want to round the result to a particular number of
decimal places.  You do this with the "quantize" function.  It takes
two Quad: one that you want to round, and another that has the
number of decimal places you want to round to.

> putStrLn "This is 10 / 6, rounded to two places:";
> BS8.putStrLn . P.evalEnv $ do
>   twoPlaces <- P.fromString . BS8.pack $ "1e-2"
>   r <- P.quantize tenThirds twoPlaces
>   P.toString r
> ;

By default, rounding is done using the "roundHalfEven" method.  You
can set a different rounding method if you wish; the rounding
methods are listed in the Haddock documentation for Data.Deka.IO.

> putStrLn "This is 10 / 6, rounded using the 'roundDown' method.";
> BS8.putStrLn . P.evalEnv $ do
>   twoPlaces <- P.fromString . BS8.pack $ "1e-2"
>   P.setRound P.roundDown
>   r <- P.quantize tenThirds twoPlaces
>   P.toString r

> };

Flags
-----

A computation may set any number of flags.  These are listed in the
Data.Deka.IO module.  Functions in Data.Deka.IO manipulate which
flags are currently set.  Though computations set flags, they never
clear them.  You have to clear them yourself.

In addition to flags being available for inspection within the Env
monad, you can get the final flags using runEnv.  FlagList gives you
a list of flags that are set.

> let (r, fl) = P.runEnv $ do
>       big1 <- P.fromString . BS8.pack $ "987e3000"
>       big2 <- P.fromString . BS8.pack $ "322e6000"
>       rslt <- P.multiply big1 big2
>       P.toString rslt
> in do
>      putStr "result: "
>      BS8.putStrLn r
>      putStr "flags set: "
>      print . P.flagList $ fl
> ;

The above example also shows that computations may return a Quad
that is not finite--that is, it might be inifite, or it might be a
Not-a-Number, or NaN.  In contrast, computations using the Deka type
never return non-finite values.

Conclusion
----------

That should be enough to get you started.  If you find any bug no
matter how small--even just a typo in the documentation--report it
to me at omari@smileystation.com or file a ticket or a pull request
in Github:

https://github.com/massysett/deka

No bug is too small!

> };

-- | Parse octothorpe-containing operands and results.

module Dectest.Parse.Octothorpe where

import qualified Deka.Internal.Decnumber.Decimal32 as D32
import qualified Deka.Internal.Decnumber.Decimal64 as D64
import qualified Deka.Internal.Decnumber.Decimal128 as D128
import Foreign.Safe

data Octo
  = Null
  | O32 (ForeignPtr D32.C'decimal32)
  | O64 (ForeignPtr D64.C'decimal64)
  | O128 (ForeignPtr D128.C'decimal128)

{- Parsing Operand Octothorpes

   1.  Octothorpe alone is a null reference.

   2.  Octothorpe followed by 8, 16, or 32 hexadecimal digits is a
   decimal16, decimal32, or decimal64, respectively.

       a.  Decode the hex digits without loss or constraint into the
       respective decimalXX format.

       b.  If the decimalXX format must be converted to the format
       being used for the test calculation, do so now.  decimalXX to
       decNumber conversions are lossless.  There are no instances
       of decimalXX formats that require conversion to another
       decimalXX format.

       c.  Apply the directives to the operand.  Apply precision
       only when the operation is toSci, toEng, or apply; otherwise,
       use sufficient precision so that rounding is avoided.
       (precision will be an issue only when converting to
       decNumber.)  This step may set flags.

   3.  2 digits followed by an octothorpe followed by a numeric
   string specifies a number in the specified format.  (These occur
   only in decNumber tests.)

       a.  Convert the numeric string to a decNumber.  Then, convert
       the resulting decNumber to the target decimalXX format.  This
       may cause rounding or other conditions; the flags are set as
       usual.

       b.  Follow step 2b and 2c above.
-}

{- Parsing Result Octothorpes

   1.  Octothorpe alone is a null reference.

   2.  Octothorpe followed by 8, 16, or 32 hexadecimal digits is a
   decimal16, decimal32, or decimal64, respectively.

       a.  Decode the hex digits without loss or constraint into the
       respective decimalXX format.

       b.  Convert the result of the test calculation to the type of
       the given decoding.  This may modify or clamp the result to
       fit the format, giving a different result than if the result
       had been expressed as a string and possibly raising new
       conditions.

   3.  2 digits followed by an octothorpe followed by a numeric
   string specifies a number in the specified format.  (These occur
   only in decNumber tests.)

       a.  The numeric string is first converted to a decNumber and
       is then encoded in the selected format.  This may cause
       rounding or other conditions, but NO conditions are set by
       this process.

       b.  Follow step 2b above.

-}

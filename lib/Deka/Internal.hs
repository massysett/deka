-- | Deka internals.
--
-- Since Deka is a binding to the decNumber C library, most of the
-- action happens in the IO monad.  Deka goes to great lengths to
-- hide the IO using 'unsafePerformIO'.  Most of the user-facing API
-- is a thin wrapper of code in the Deka.Internal tree.  By keeping
-- the internals here, they are easier to view in Haddock.
--
-- By keeping most of the modules Safe (from a Safe Haskell
-- perspective) they are easier to test.  Though Deka uses
-- 'unsafePerformIO' only as appropriate, its use makes testing more
-- cumbersome, as functions that use 'unsafePerformIO' are lying
-- about their type.  By keeping most impure work in functions with
-- IO signatures and then using 'unsafePerformIO' only in thin
-- wrappers, it's easier to test the impure work and be sure that no
-- visible side effects leak from these functions.
--
-- You shouldn't need to use any of the functions in here; you
-- shouldn't even need to look at them.  But here they are if you
-- wish to examine them.
module Deka.Internal where
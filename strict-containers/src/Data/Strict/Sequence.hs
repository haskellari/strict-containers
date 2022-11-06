{- | Fully-strict version of "Data.Sequence"

Unlike "Data.Sequence" t'Data.Sequence.Seq' the instances of our {- haddock
 #1251 -} t'Data.Strict.Sequence.Seq' are all strict as well.

You should be able to switch from the former simply by changing your module
imports, and your package dependency from @containers@ to @strict-containers@.
If this doesn't work, please file a bug.

The documentation in the auto-generated modules have not been updated in a
particularly sophisticated way, so may sound weird or contradictory. In those
cases, please re-interpret such weird wording in the context of the above.

== Detailed note on laziness

'Seq' uses internal laziness for performance; and our data structure preserves
this laziness and performance in a way that retains the strictness of values.
For technical details, see the source code of our patch. As a user of the data
structure, what you need to know is that:

* Strictness is guaranteed when constructing containers - values added to a
  container are evaluated /before/ the new, larger, container itself is
  evaluated.

* Laziness and performance applies when splitting or combining existing
  containers, whose values have already been evaluated as per the previous
  point.

== Bugs

One known bug, is that whole-container transforms (such as @fmap@) are not
entirely strict, since they make use of the lazy behaviour above to avoid doing
work that is unnecessary (in the lazy case) to a large part of the data
structure. This is possible to fix, by re-implementing all such transforms so
that they force the lazy parts as well; we just haven't gotten around to it
yet. (This would revert the performance back to @O(n)@, but this is unavoidable
since all such transforms on strict data structures must inherently evaluate
every single element.)

-}
module Data.Strict.Sequence
  ( module Data.Strict.Sequence.Autogen
  ) where

import Data.Strict.Sequence.Internal
import Data.Strict.Sequence.Autogen

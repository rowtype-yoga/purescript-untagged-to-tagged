module Data.UntaggedToTagged.Discriminated.RepOneOf
  ( class RepOneOf
  , oneOfToRep
  , repToOneOf
  )
  where

import Prelude

import Data.Generic.Rep (Argument(..), Constructor(..), Sum(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.UntaggedToTagged.Discriminated.Options (class OptTransform, MkOpts, MkTag, Opts, applyOpts, revertOpts)
import Data.UntaggedToTagged.Discriminated.Types (StrLit, unStrLit)
import Prim.Row as Row
import Record as Record
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (class InOneOf, OneOf, asOneOf)


--------------------------------------------------------------------------------
--- RepOneOf
--------------------------------------------------------------------------------

class RepOneOf :: Opts -> Type -> Type -> Constraint
class
  RepOneOf opts oneOf rep
  | rep -> oneOf

  where
  oneOfToRep :: Proxy opts -> oneOf -> rep
  repToOneOf :: Proxy opts -> rep -> oneOf

instance
  ( Row.Cons symTag (StrLit caseTag) fx f
  , OptTransform opts (Record rin) a
  , TypeEquals opts (MkOpts (MkTag symTag) bx)
  , Row.Cons symTag (StrLit caseTag) trash (rin)

  ) =>
  RepOneOf
    opts
    (Record rin)
    (Constructor caseTag (Argument a))
  where
  oneOfToRep prxOpts =
    Constructor <<< Argument <<< applyOpts prxOpts

  repToOneOf prxOpts (Constructor (Argument x)) =
    revertOpts prxOpts x

instance
  ( RepOneOf opts (Record leftUnion) leftADT
  , RepOneOf opts rightUnion rightADT
  , TypeEquals opts (MkOpts (MkTag symTag) optsRest)
  , Row.Cons symTag (StrLit symCase) trash leftUnion
  , IsSymbol symTag
  , IsSymbol symCase
  , TypeEquals leftADT (Constructor symCase s)

  , InOneOf rightUnion (Record leftUnion) rightUnion
  ) =>
  RepOneOf
    opts
    (OneOf (Record leftUnion) rightUnion)
    (Sum leftADT rightADT)
  where
  oneOfToRep prxOpts untagged =
    if tagCandidate == tagRuntime then
      Inl $
        oneOfToRep prxOpts (unsafeCoerce $ untagged :: Record leftUnion)
    else
      Inr $
        oneOfToRep prxOpts (unsafeCoerce $ untagged :: rightUnion)

    where
    tagCandidate =
      reflectSymbol prxSymCase

    tagRuntime =
      unStrLit $ Record.get prxSymTag (unsafeCoerce untagged :: Record leftUnion)

    prxSymTag = Proxy :: _ symTag
    prxSymCase = Proxy :: _ symCase

  repToOneOf prxOpts =
    case _ of
      Inl x -> asOneOf $ repToOneOf prxOpts x
      Inr x -> asOneOf $ repToOneOf prxOpts x


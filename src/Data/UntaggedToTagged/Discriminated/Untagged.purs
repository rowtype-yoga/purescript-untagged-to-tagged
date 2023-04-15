module Data.UntaggedToTagged.Discriminated.Untagged where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), to)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.UntaggedToTagged.Discriminated.Options (class OptTranform, MkOpts, MkTag, Opts, optTransform)
import Literals (StringLit, toValue)
import Prim.Row as Row
import Record as Record
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (OneOf)

class UntaggedByHelper :: Opts -> Type -> Type -> Constraint
class UntaggedByHelper opts untagged tagged | untagged -> tagged where
  toTaggedHelper :: Proxy opts -> untagged -> tagged

instance
  ( Row.Cons symTag (StringLit caseTag) fx f
  , OptTranform opts (Record rin) a
  , IsSymbol symTag
  , IsSymbol caseTag
  , TypeEquals opts (MkOpts (MkTag symTag) bx)
  ) =>
  UntaggedByHelper
    opts
    (Record rin)
    (Constructor caseTag (Argument a))
  where
  toTaggedHelper prxOpts =
    Constructor <<< Argument <<< optTransform prxOpts

instance
  ( UntaggedByHelper opts (Record leftUnion) leftADT
  , UntaggedByHelper opts rightUnion rightADT
  , TypeEquals opts (MkOpts (MkTag symTag) optsRest)
  , Row.Cons symTag (StringLit symCase) trash leftUnion
  , IsSymbol symTag
  , IsSymbol symCase
  ) =>
  UntaggedByHelper
    opts
    (OneOf (Record leftUnion) rightUnion)
    (Sum leftADT rightADT)
  where
  toTaggedHelper prxOpts untagged =
    if tagCandidate == tagRuntime then
      Inl $
        toTaggedHelper prxOpts (unsafeCoerce $ untagged :: Record leftUnion)
    else
      Inr $
        toTaggedHelper prxOpts (unsafeCoerce $ untagged :: rightUnion)

    where
    tagCandidate =
      reflectSymbol prxSymCase

    tagRuntime =
      toValue $ Record.get prxSymTag (unsafeCoerce untagged :: Record leftUnion)

    prxSymTag = Proxy :: _ symTag
    prxSymCase = Proxy :: _ symCase

class UntaggedBy :: Opts -> Type -> Type -> Constraint
class UntaggedBy opts untagged tagged where
  -- | Convert an untagged union to a tagged union. E.g. 
  -- | 
  -- | ```purescript
  -- | ...
  -- | ```
  toTaggedBy :: Proxy opts -> untagged -> tagged

instance
  ( Generic tagged taggedGen
  , UntaggedByHelper opts untagged taggedGen
  ) =>
  UntaggedBy opts untagged tagged where
  toTaggedBy prxOpts = toTaggedHelper prxOpts >>> to

module Data.UntaggedToTagged.Untagged where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), to)
import Data.UntaggedToTagged.Options (class ApplyOpts, Opts, applyOpts)
import Type.Data.List (Nil')
import Type.Proxy (Proxy(..))
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (OneOf, toEither1)

class UntaggedHelper :: Opts -> Type -> Type -> Constraint
class UntaggedHelper opts untagged tagged | untagged -> tagged where
  toTaggedHelper :: Proxy opts -> untagged -> tagged

instance
  ( HasRuntimeType l
  , ApplyOpts opts l l'
  , UntaggedHelper opts (OneOf ln rn) next
  ) =>
  UntaggedHelper opts (OneOf l (OneOf ln rn)) (Sum (Constructor sym (Argument l')) next)
  where
  toTaggedHelper prxOpts untagged =
    case toEither1 untagged of
      Left l -> Inl (Constructor (Argument $ applyOpts prxOpts l))
      Right r -> Inr $ toTaggedHelper prxOpts r

else instance
  ( HasRuntimeType l
  , ApplyOpts opts l l'
  , ApplyOpts opts r r'
  ) =>
  UntaggedHelper opts (OneOf l r) (Sum (Constructor syml (Argument l')) (Constructor symr (Argument r'))) where
  toTaggedHelper prxOpts untagged =
    case toEither1 untagged of
      Left l -> Inl (Constructor (Argument $ applyOpts prxOpts l))
      Right r -> Inr (Constructor (Argument $ applyOpts prxOpts r))

class Untagged :: Type -> Type -> Constraint
class Untagged untagged tagged where
  -- | Convert an untagged union to a tagged union. E.g. 
  -- | 
  -- | ```purescript
  -- | type ISU = Int |+| String 
  -- | 
  -- | data IST = IT Int | ST String 
  -- | derive instance Generic IST _ 
  -- | 
  -- | isu :: ISU
  -- | isu = asOneOf "Wurst"
  -- | 
  -- | ist :: IST 
  -- | ist = toTagged isu
  -- | -- (ST "Wurst")
  -- | ```
  toTagged :: untagged -> tagged

instance (Generic tagged taggedGen, UntaggedHelper Nil' untagged taggedGen) => Untagged untagged tagged where
  toTagged = toTaggedHelper (Proxy :: _ Nil') >>> to

class UntaggedWithOpts :: Opts -> Type -> Type -> Constraint
class UntaggedWithOpts opts untagged tagged where
  -- | Convert an untagged union to a tagged union. E.g. 
  -- | 
  -- | ```purescript
  -- | type ISU = Int |+| String 
  -- | 
  -- | data IST = IT Int | ST String 
  -- | derive instance Generic IST _ 
  -- | 
  -- | isu :: ISU
  -- | isu = asOneOf "Wurst"
  -- | 
  -- | ist :: IST 
  -- | ist = toTagged isu
  -- | -- (ST "Wurst")
  -- | ```
  toTaggedWithOpts :: Proxy opts -> untagged -> tagged

instance (Generic tagged taggedGen, UntaggedHelper opts untagged taggedGen) => UntaggedWithOpts opts untagged tagged where
  toTaggedWithOpts prxOpts = toTaggedHelper prxOpts >>> to


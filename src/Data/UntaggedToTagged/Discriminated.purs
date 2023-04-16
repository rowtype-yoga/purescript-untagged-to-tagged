module Data.UntaggedToTagged.Discriminated where

import Prelude

import Data.Generic.Rep (class Generic, from, to)
import Data.UntaggedToTagged.Discriminated.Options (Opts)
import Data.UntaggedToTagged.Discriminated.RepOneOf (class RepOneOf, oneOfToRep, repToOneOf)
import Type.Proxy (Proxy)

class Discriminated :: Opts -> Type -> Type -> Constraint
class Discriminated opts untagged tagged where
  -- | Convert an untagged union to a tagged union with discrimination options.
  toTaggedBy :: Proxy opts -> untagged -> tagged

  -- | Convert a tagged union to an untagged union with discrimination options.
  fromTaggedBy :: Proxy opts -> tagged -> untagged

instance
  ( Generic tagged taggedGen
  , RepOneOf opts untagged taggedGen
  ) =>
  Discriminated opts untagged tagged where

  toTaggedBy prxOpts =
    oneOfToRep prxOpts >>> to

  fromTaggedBy prxOpts =
    repToOneOf prxOpts <<< from

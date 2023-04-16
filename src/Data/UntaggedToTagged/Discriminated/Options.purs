module Data.UntaggedToTagged.Discriminated.Options where

import Prelude

import Data.Symbol (class IsSymbol)
import Literals (StringLit, stringLit)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))

foreign import data Opts :: Type

foreign import data Shape :: Type

foreign import data Tag :: Type

foreign import data MkTag :: Symbol -> Tag

foreign import data MkShapeFlat :: Shape

foreign import data MkShapeNested :: Symbol -> Shape

foreign import data MkOpts :: Tag -> Shape -> Opts

---

class OptTransform :: Opts -> Type -> Type -> Constraint
class OptTransform opts a b where
  applyOpts :: Proxy opts -> a -> b
  revertOpts :: Proxy opts -> b -> a

instance
  ( Row.Cons symTag (StringLit symCase) rb ra
  , Row.Lacks symTag rb
  , IsSymbol symTag
  , IsSymbol symCase
  ) =>
  OptTransform (MkOpts (MkTag symTag) MkShapeFlat) (Record ra) (Record rb)
  where

  applyOpts _ =
    Record.delete prxSymTag
    where
    prxSymTag = Proxy :: _ symTag

  revertOpts _ =
    Record.insert prxSymTag stringLitCase
    where
    prxSymTag = Proxy :: _ symTag
    stringLitCase = stringLit :: _ symCase

instance
  ( Row.Cons symTag (StringLit symCase) () ra'
  , Row.Lacks symVal ra'
  , Row.Cons symVal b ra' ra
  , IsSymbol symVal
  , IsSymbol symTag
  , IsSymbol symCase
  ) =>
  OptTransform (MkOpts (MkTag symTag) (MkShapeNested symVal)) (Record ra) b
  where

  applyOpts _ =
    Record.get prxSymVal
    where
    prxSymVal = Proxy :: _ symVal

  revertOpts _ val =
    {}
      # Record.insert prxSymTag stringLitCase
      # Record.insert prxSymVal val
    where
    prxSymVal = Proxy :: _ symVal
    prxSymTag = Proxy :: _ symTag
    stringLitCase = stringLit :: _ symCase


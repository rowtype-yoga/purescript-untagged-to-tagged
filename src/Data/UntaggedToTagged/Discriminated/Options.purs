module Data.UntaggedToTagged.Discriminated.Options where

import Data.Symbol (class IsSymbol)
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

class OptTranform :: Opts -> Type -> Type -> Constraint
class OptTranform opts tin tout  where
  optTransform :: Proxy opts -> tin -> tout

instance
  ( Row.Cons tagSym a rout rin
  , IsSymbol tagSym
  , Row.Lacks tagSym rout
  ) =>
  OptTranform (MkOpts (MkTag tagSym) MkShapeFlat) (Record rin) (Record rout)
  where
  optTransform _ = Record.delete prxTagSym
    where
    prxTagSym = Proxy :: _ tagSym

instance
  ( Row.Cons symVal tout rx rin
  , IsSymbol symVal
  ) =>
  OptTranform (MkOpts (MkTag symTag) (MkShapeNested symVal)) (Record rin) tout
  where
  optTransform _ = Record.get prxSym
    where
    prxSym = Proxy :: _ symVal

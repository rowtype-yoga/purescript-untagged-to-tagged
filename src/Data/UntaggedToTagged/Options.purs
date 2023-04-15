module Data.UntaggedToTagged.Options where

import Prelude

import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Record as Record
import Type.Data.List (Cons', List', Nil')
import Type.Proxy (Proxy(..))

type Opts = List' Opt

foreign import data Opt :: Type

foreign import data StripTag :: Symbol -> Opt

foreign import data ValueFrom :: Symbol -> Opt

---

class ApplyOpts :: List' Opt -> Type -> Type -> Constraint
class ApplyOpts opts tin tout | opts tin -> tout where
  applyOpts :: Proxy opts -> tin -> tout

instance ApplyOpts Nil' a a
  where
  applyOpts _ = identity

instance
  ( ApplyOpt opt toutPrev tout
  , ApplyOpts optsTail tin toutPrev
  ) =>
  ApplyOpts (Cons' opt optsTail) tin tout
  where
  applyOpts _ = applyOpts prxOptsTail >>> applyOpt prxOpt
    where
    prxOpt = Proxy :: _ opt
    prxOptsTail = Proxy :: _ optsTail

---

class ApplyOpt :: Opt -> Type -> Type -> Constraint
class ApplyOpt opt tin tout | opt tin -> tout where
  applyOpt :: Proxy opt -> tin -> tout

instance
  ( Row.Cons sym a rout rin
  , IsSymbol sym
  , Row.Lacks sym rout
  ) =>
  ApplyOpt (StripTag sym) (Record rin) (Record rout)
  where
  applyOpt _ = Record.delete prxSym
    where
    prxSym = Proxy :: _ sym

instance
  ( Row.Cons sym (Record rout) rx rin
  , IsSymbol sym
  ) =>
  ApplyOpt (ValueFrom sym) (Record rin) (Record rout)
  where
  applyOpt _ = Record.get prxSym
    where
    prxSym = Proxy :: _ sym
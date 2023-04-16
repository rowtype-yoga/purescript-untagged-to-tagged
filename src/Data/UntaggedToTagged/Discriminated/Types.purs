module Data.UntaggedToTagged.Discriminated.Types where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign as F
import Type.Proxy (Proxy(..))
import Untagged.TypeCheck (class HasRuntimeType)

newtype StrLit :: Symbol -> Type
newtype StrLit sym = StrLit String

unStrLit :: forall sym. StrLit sym -> String
unStrLit (StrLit str) = str

strLit :: forall sym. IsSymbol sym => StrLit sym
strLit = StrLit (reflectSymbol (Proxy :: _ sym))

instance IsSymbol sym => HasRuntimeType (StrLit sym)
  where
  hasRuntimeType _ foreign_ =
    case runExcept $ F.readString foreign_ of
      Left _ -> false
      Right str -> str == reflectSymbol prxSym
    where
    prxSym = Proxy :: _ sym

derive newtype instance Eq (StrLit sym)


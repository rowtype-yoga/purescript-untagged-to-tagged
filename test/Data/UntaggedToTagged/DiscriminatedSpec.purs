module Test.Data.UntaggedToTagged.DiscriminatedSpec where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.UntaggedToTagged.Discriminated (fromTaggedBy, toTaggedBy)
import Data.UntaggedToTagged.Discriminated.Options (MkOpts, MkShapeFlat, MkShapeNested, MkTag)
import Foreign as F
import Literals (StringLit, stringLit)
import Pipes.Core (reflect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (AnyShow(..), shouldEqual)
import Type.Proxy (Proxy(..))
import Untagged.TypeCheck (class HasRuntimeType, hasRuntimeType)
import Untagged.Union (type (|+|), asOneOf)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data Wrap a = Wrap a

instance IsSymbol sym => HasRuntimeType (Wrap (StringLit sym))
  where
  hasRuntimeType prx foreign_ =
    case runExcept $ F.readString foreign_ of
      Left _ -> false
      Right str -> str == reflectSymbol prxSym
    where
    prxSym = Proxy :: _ sym

--- | Sample type with nested discrimination encoding
type OneOfRemoteDataNested =
  { kind :: StringLit "Success"
  , value ::
      { result :: String }
  }
    |+|
      { kind :: StringLit "Failure"
      , value ::
          { errorMsg :: String
          , errorCode :: Int
          }
      }
    |+|
      { kind :: StringLit "Loading"
      , value ::
          { progress :: Number }
      }

--- | Sample type with flat discrimination encoding
type OneOfRemoteDataFlat =
  { kind :: StringLit "Success"
  , result :: String
  }
    |+|
      { kind :: StringLit "Failure"
      , errorMsg :: String
      , errorCode :: Int
      }
    |+|
      { kind :: StringLit "Loading"
      , progress :: Number
      }

--- | Sample type native ADT
data ADTRemoteData
  = Success
      { result :: String }
  | Failure
      { errorMsg :: String
      , errorCode :: Int
      }
  | Loading
      { progress :: Number
      }

--------------------------------------------------------------------------------
--- Spec
--------------------------------------------------------------------------------

spec :: Spec Unit
spec =
  describe "Data.UntaggedToTagged.Discriminated" do

    describe "toTaggedBy" do
      it "should convert an untagged to a tagged union with options" do
        let
          opts =
            Proxy
              :: _ (MkOpts (MkTag "kind") MkShapeFlat)

          oneOf1 :: OneOfRemoteDataFlat
          oneOf1 = asOneOf
            { kind: stringLit :: StringLit "Success"
            , result: "one-two-three"
            }

          oneOf2 :: OneOfRemoteDataFlat
          oneOf2 = asOneOf
            { kind: stringLit :: StringLit "Failure"
            , errorCode: 12
            , errorMsg: "dooh!"
            }

          oneOf3 :: OneOfRemoteDataFlat
          oneOf3 = asOneOf
            { kind: stringLit :: StringLit "Loading"
            , progress: 99.9
            }

          adt1 :: ADTRemoteData
          adt1 = Success { result: "one-two-three" }

          adt2 :: ADTRemoteData
          adt2 = Failure { errorMsg: "dooh!", errorCode: 12 }

          adt3 :: ADTRemoteData
          adt3 = Loading { progress: 99.9 }

        --(AnyShow $ fromTaggedBy opts adt1) `shouldEqual` (AnyShow oneOf1)

        (fromTaggedBy opts adt1 # toTaggedBy opts) `shouldEqual` adt1
        (fromTaggedBy opts adt2 # toTaggedBy opts) `shouldEqual` adt2
        (fromTaggedBy opts adt3 # toTaggedBy opts) `shouldEqual` adt3

      it "should convert an untagged to a tagged union with options" do
        let
          opts =
            Proxy
              :: _ (MkOpts (MkTag "kind") (MkShapeNested "value"))

          oneOf1 :: OneOfRemoteDataNested
          oneOf1 = asOneOf
            { kind: stringLit :: StringLit "Success"
            , value:
                { result: "one-two-three" }
            }

          oneOf2 :: OneOfRemoteDataNested
          oneOf2 = asOneOf
            { kind: stringLit :: StringLit "Failure"
            , value:
                { errorCode: 12
                , errorMsg: "dooh!"
                }
            }

          oneOf3 :: OneOfRemoteDataNested
          oneOf3 = asOneOf
            { kind: stringLit :: StringLit "Loading"
            , value:
                { progress: 99.9 }
            }

          adt1 :: ADTRemoteData
          adt1 = Success { result: "one-two-three" }

          adt2 :: ADTRemoteData
          adt2 = Failure { errorMsg: "dooh!", errorCode: 12 }

          adt3 :: ADTRemoteData
          adt3 = Loading { progress: 99.9 }

        (fromTaggedBy opts adt1 # toTaggedBy opts) `shouldEqual` adt1
        (fromTaggedBy opts adt2 # toTaggedBy opts) `shouldEqual` adt2
        (fromTaggedBy opts adt3 # toTaggedBy opts) `shouldEqual` adt3

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

derive instance Generic ADTRemoteData _

instance Show ADTRemoteData where
  show = genericShow

derive instance Eq ADTRemoteData

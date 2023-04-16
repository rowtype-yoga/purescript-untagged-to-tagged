module Test.Data.UntaggedToTagged.DiscriminatedSpec where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.UntaggedToTagged.Discriminated.Options (MkOpts, MkShapeFlat, MkShapeNested, MkTag)
import Data.UntaggedToTagged.Discriminated.Untagged (fromTaggedBy, toTaggedBy)
import Literals (StringLit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (AnyShow(..), shouldEqual)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (type (|+|), asOneOf)

type UnionRemoteDataNested =
  { kind :: StringLit "Success"
  , value ::
      { result :: Array String }
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

type UnionRemoteDataFlat =
  { kind :: StringLit "Success"
  , result :: Array String
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

data ADTRemoteData
  = Success
      { result :: Array String }
  | Failure
      { errorMsg :: String
      , errorCode :: Int
      }
  | Loading
      { progress :: Number
      }

derive instance Generic ADTRemoteData _

instance Show ADTRemoteData where
  show = genericShow

derive instance Eq ADTRemoteData

spec :: Spec Unit
spec =
  describe "Data.UntaggedToTagged.Discriminated" do

    describe "toTaggedBy" do
      it "should convert an untagged to a tagged union with options" do
        let
          opts =
            Proxy
              :: _ (MkOpts (MkTag "kind") MkShapeFlat)

          x1 :: UnionRemoteDataFlat
          x1 = asOneOf
            { kind: unsafeCoerce 1 :: StringLit "Success"
            , result: [ "one", "two", "three" ]
            }

          x2 :: UnionRemoteDataFlat
          x2 = asOneOf
            { kind: unsafeCoerce 1 :: StringLit "Failure"
            , errorCode: 12
            , errorMsg: "dooh!"
            }

          x3 :: UnionRemoteDataFlat
          x3 = asOneOf
            { kind: unsafeCoerce 1 :: StringLit "Loading"
            , progress: 99.9
            }

          y1 :: ADTRemoteData
          y1 = Success { result: [ "one", "two", "three" ] }

          y2 :: ADTRemoteData
          y2 = Failure { errorMsg: "dooh!", errorCode: 12 }

          y3 :: ADTRemoteData
          y3 = Loading { progress: 99.9 }

          z1 :: UnionRemoteDataFlat
          z1 = fromTaggedBy opts y1

          -- s :: ADTRemoteData
          -- s = toTaggedBy opts x1

        (toTaggedBy opts x1) `shouldEqual` y1
        (toTaggedBy opts x2) `shouldEqual` y2
        (toTaggedBy opts x3) `shouldEqual` y3
        (AnyShow $ z) `shouldEqual` (AnyShow x1)

      it "should convert an untagged to a tagged union with options" do
        let
          opts =
            Proxy
              :: _ (MkOpts (MkTag "kind") (MkShapeNested "value"))

          x1 :: UnionRemoteDataNested
          x1 = asOneOf
            { kind: unsafeCoerce 1 :: StringLit "Success"
            , value:
                { result: [ "one", "two", "three" ] }
            }

          x2 :: UnionRemoteDataNested
          x2 = asOneOf
            { kind: unsafeCoerce 1 :: StringLit "Failure"
            , value:
                { errorCode: 12
                , errorMsg: "dooh!"
                }
            }

          x3 :: UnionRemoteDataNested
          x3 = asOneOf
            { kind: unsafeCoerce 1 :: StringLit "Loading"
            , value:
                { progress: 99.9 }
            }

          y1 :: ADTRemoteData
          y1 = Success { result: [ "one", "two", "three" ] }

          y2 :: ADTRemoteData
          y2 = Failure { errorMsg: "dooh!", errorCode: 12 }

          y3 :: ADTRemoteData
          y3 = Loading { progress: 99.9 }

        (toTaggedBy opts x1) `shouldEqual` y1
        (toTaggedBy opts x2) `shouldEqual` y2
        (toTaggedBy opts x3) `shouldEqual` y3

opts =
  Proxy
    :: _ (MkOpts (MkTag "kind") MkShapeFlat)

x1 :: UnionRemoteDataFlat
x1 = asOneOf
  { kind: unsafeCoerce 1 :: StringLit "Success"
  , result: [ "one", "two", "three" ]
  }

x2 :: UnionRemoteDataFlat
x2 = asOneOf
  { kind: unsafeCoerce 1 :: StringLit "Failure"
  , errorCode: 12
  , errorMsg: "dooh!"
  }

x3 :: UnionRemoteDataFlat
x3 = asOneOf
  { kind: unsafeCoerce 1 :: StringLit "Loading"
  , progress: 99.9
  }

y1 :: ADTRemoteData
y1 = Success { result: [ "one", "two", "three" ] }

y2 :: ADTRemoteData
y2 = Failure { errorMsg: "dooh!", errorCode: 12 }

y3 :: ADTRemoteData
y3 = Loading { progress: 99.9 }


z :: _
z = fromTaggedBy opts y1
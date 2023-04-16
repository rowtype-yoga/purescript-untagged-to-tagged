module Test.Data.UntaggedToTagged.DiscriminatedSpec where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.UntaggedToTagged.Discriminated (fromTaggedBy, toTaggedBy)
import Data.UntaggedToTagged.Discriminated.Options (MkOpts, MkShapeFlat, MkShapeNested, MkTag)
import Data.UntaggedToTagged.Discriminated.Types (StrLit, strLit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (AnyShow(..), shouldEqual)
import Type.Proxy (Proxy(..))
import Untagged.Union (type (|+|), asOneOf)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

--- | Sample type with nested discrimination encoding
type OneOfRemoteDataNested =
  { kind :: StrLit "Success"
  , value ::
      { result :: String }
  }
    |+|
      { kind :: StrLit "Failure"
      , value ::
          { errorMsg :: String
          , errorCode :: Int
          }
      }
    |+|
      { kind :: StrLit "Loading"
      , value ::
          { progress :: Number }
      }

--- | Sample type with flat discrimination encoding
type OneOfRemoteDataFlat =
  { kind :: StrLit "Success"
  , result :: String
  }
    |+|
      { kind :: StrLit "Failure"
      , errorMsg :: String
      , errorCode :: Int
      }
    |+|
      { kind :: StrLit "Loading"
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

    describe "flat encoding" do
      let
        opts =
          Proxy
            :: _ (MkOpts (MkTag "kind") MkShapeFlat)

        oneOf1 :: OneOfRemoteDataFlat
        oneOf1 = asOneOf
          { kind: strLit :: StrLit "Success"
          , result: "one-two-three"
          }

        oneOf2 :: OneOfRemoteDataFlat
        oneOf2 = asOneOf
          { kind: strLit :: StrLit "Failure"
          , errorCode: 12
          , errorMsg: "dooh!"
          }

        oneOf3 :: OneOfRemoteDataFlat
        oneOf3 = asOneOf
          { kind: strLit :: StrLit "Loading"
          , progress: 99.9
          }

        adt1 :: ADTRemoteData
        adt1 = Success { result: "one-two-three" }

        adt2 :: ADTRemoteData
        adt2 = Failure { errorMsg: "dooh!", errorCode: 12 }

        adt3 :: ADTRemoteData
        adt3 = Loading { progress: 99.9 }

      describe "fromTaggedBy" do
        it "should convert a tagged to an untagged union with discrimination options" do

          (AnyShow $ fromTaggedBy opts adt1) `shouldEqual` (AnyShow oneOf1)
          (AnyShow $ fromTaggedBy opts adt2) `shouldEqual` (AnyShow oneOf2)
          (AnyShow $ fromTaggedBy opts adt3) `shouldEqual` (AnyShow oneOf3)

      describe "toTaggedBy" do

        it "should convert an untagged to a tagged union with discrimination options" do

          (AnyShow $ toTaggedBy opts oneOf1) `shouldEqual` (AnyShow adt1)
          (AnyShow $ toTaggedBy opts oneOf2) `shouldEqual` (AnyShow adt2)
          (AnyShow $ toTaggedBy opts oneOf3) `shouldEqual` (AnyShow adt3)

    describe "nested encodiing" do

      let
        opts =
          Proxy
            :: _ (MkOpts (MkTag "kind") (MkShapeNested "value"))

        oneOf1 :: OneOfRemoteDataNested
        oneOf1 = asOneOf
          { kind: strLit :: StrLit "Success"
          , value:
              { result: "one-two-three" }
          }

        oneOf2 :: OneOfRemoteDataNested
        oneOf2 = asOneOf
          { kind: strLit :: StrLit "Failure"
          , value:
              { errorCode: 12
              , errorMsg: "dooh!"
              }
          }

        oneOf3 :: OneOfRemoteDataNested
        oneOf3 = asOneOf
          { kind: strLit :: StrLit "Loading"
          , value:
              { progress: 99.9 }
          }

        adt1 :: ADTRemoteData
        adt1 = Success { result: "one-two-three" }

        adt2 :: ADTRemoteData
        adt2 = Failure { errorMsg: "dooh!", errorCode: 12 }

        adt3 :: ADTRemoteData
        adt3 = Loading { progress: 99.9 }

      describe "fromTaggedBy" do
        it "should convert a tagged to an untagged union with discrimination options" do

          (AnyShow $ fromTaggedBy opts adt1) `shouldEqual` (AnyShow oneOf1)
          (AnyShow $ fromTaggedBy opts adt2) `shouldEqual` (AnyShow oneOf2)
          (AnyShow $ fromTaggedBy opts adt3) `shouldEqual` (AnyShow oneOf3)

      describe "toTaggedBy" do

        it "should convert an untagged to a tagged union with discrimination options" do

          (AnyShow $ toTaggedBy opts oneOf1) `shouldEqual` (AnyShow adt1)
          (AnyShow $ toTaggedBy opts oneOf2) `shouldEqual` (AnyShow adt2)
          (AnyShow $ toTaggedBy opts oneOf3) `shouldEqual` (AnyShow adt3)

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

derive instance Generic ADTRemoteData _

derive instance Eq ADTRemoteData

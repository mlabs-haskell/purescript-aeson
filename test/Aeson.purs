module Test.Aeson where

import Prelude

import Aeson
  ( Aeson
  , JsonDecodeError(..)
  , aesonNull
  , caseAesonArray
  , caseAesonBigInt
  , caseAesonBigNumber
  , caseAesonBoolean
  , caseAesonFiniteBigNumber
  , caseAesonFiniteNumber
  , caseAesonInt
  , caseAesonNull
  , caseAesonObject
  , caseAesonString
  , caseAesonUInt
  , decodeAeson
  , decodeJsonString
  , encodeAeson
  , finiteBigNumber
  , fromArray
  , fromBigInt
  , fromBoolean
  , fromFiniteBigNumber
  , fromInt
  , fromObject
  , fromString
  , fromUInt
  , getField
  , getFieldOptional
  , getFieldOptional'
  , getNestedAeson
  , isArray
  , isBigInt
  , isBigNumber
  , isBoolean
  , isInt
  , isNull
  , isNumber
  , isObject
  , isString
  , isUInt
  , jsonToAeson
  , parseJsonStringToAeson
  , stringifyAeson
  , toArray
  , toBigInt
  , toBigNumber
  , toBoolean
  , toInt
  , toNull
  , toNumber
  , toObject
  , toString
  , toStringifiedNumbersJson
  , toUInt
  , unpackFinite
  )
import Control.Lazy (fix)
import Data.Argonaut (Json, caseJson)
import Data.Argonaut as Argonaut
import Data.BigNumber as BigNumber
import Data.BooleanAlgebra (implies)
import Data.Either (Either(..), fromRight, hush, isLeft)
import Data.Foldable (all)
import Data.Map.Gen as Map
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Number as Number
import Data.Set as Set
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)
import Data.UInt as UInt
import Mote (test)
import Partial.Unsafe (unsafePartial)
import Test.Gen
  ( aesonArrayGen
  , aesonBigNumberGen
  , aesonGen
  , aesonNullGen
  , aesonObjectGen
  , aesonStringGen
  , bigNumberStrGen
  , defaultNumberGenConf
  , finiteBigNumberGen
  , jsonGen
  , oneOf
  , sizedArray
  )
import Test.QuickCheck (arbitrary, quickCheckGen')
import Test.Spec.Assertions (shouldEqual)
import Test.TestM (TestPlanM)
import Test.Utils (assertTrue_)

iff :: Boolean -> Boolean -> Boolean
iff a b = (a `implies` b) && (b `implies` a)

---

suite :: TestPlanM Unit
suite = do
  test "Incorrect JSON" $ do
    let
      error :: Either JsonDecodeError Int
      error = Left $ TypeMismatch "JSON String"
    decodeJsonString "{" `shouldEqual` error

  -- Round trip Aeson -> String -> Aeson should be id
  test "Roud trip: Aeson -> String -> Aeson" $
    quickCheckGen' 10000 do
      aeson <- aesonGen

      pure $ Right aeson == parseJsonStringToAeson (stringifyAeson aeson)

  -- Test parser quirks
  -- Parser should parse exactly same number as parseBigNumber
  -- or fail in case number is Infinite
  test "JS-side parser parse only finite and non-nan numbers" $
    quickCheckGen' 10000 do
      numberStr <- bigNumberStrGen defaultNumberGenConf

      let
        aesonFromFbn = do
          bn <- hush $ BigNumber.parseBigNumber numberStr
          fromFiniteBigNumber <$> finiteBigNumber bn

        aesonFromStr = hush (parseJsonStringToAeson numberStr)

      pure $ aesonFromFbn == aesonFromStr

  -- Round trip (Aeson ->) String -> Aeson -> String
  -- Precondition: initial string obtained from stringifying
  -- of Aeson. This seems to be redundant as we have "proof" of
  -- Aeson -> String -> Aeson round trip
  -- But, in practice equal Aesons can produce different string
  -- because Aeson equality is structural. I.e. {a: 0, b: 0} == {b: 0, a: 0}
  -- In fact, this test ensures that stringify does not change objects
  -- keys order, etc.
  test "Round trip: (Aeson ->) String -> Aeson -> String" $
    quickCheckGen' 10000 do
      aeson <- aesonGen
      let aesonStr = stringifyAeson aeson
      pure $ (stringifyAeson <$> parseJsonStringToAeson aesonStr) == Right aesonStr

  -- "X encoding/decoding and functions coherence" test family
  -- checks that functions like isX, toX, caseAesonX are coherent
  -- and (caseAesonX _ fromX) is id
  test "Null encoding/decoding and functions coherence"
    $ assertTrue_
    $ isNull aesonNull
        && isJust (toNull aesonNull)
        && caseAesonNull Nothing (Just <<< const aesonNull) aesonNull == Just aesonNull

  test "Boolean encoding/decoding and functions coherence" do
    let
      aesonTrue = fromBoolean true
      aesonFalse = fromBoolean false

    assertTrue_ $ isBoolean aesonTrue
      && isJust (toBoolean aesonTrue)
      && caseAesonBoolean Nothing (Just <<< fromBoolean) aesonTrue == Just aesonTrue

    assertTrue_ $ isBoolean aesonFalse
      && isJust (toBoolean aesonFalse)
      && caseAesonBoolean Nothing (Just <<< fromBoolean) aesonFalse == Just aesonFalse

  test "String encoding/decoding and functions coherence" $
    quickCheckGen' 10000 do
      aeson <- aesonStringGen
      pure $ isString aeson
        && isJust (toString aeson)
        && caseAesonString Nothing (Just <<< fromString) aeson == Just aeson

  test "Array encoding/decoding and functions coherence" $
    quickCheckGen' 10000 do
      aeson <- aesonArrayGen aesonGen
      pure $ isArray aeson
        && isJust (toArray aeson)
        && caseAesonArray Nothing (Just <<< fromArray) aeson == Just aeson

  test "Object encoding/decoding and functions coherence" $
    quickCheckGen' 10000 do
      aeson <- aesonObjectGen (Tuple <$> arbitrary <*> aesonGen)
      pure $ isObject aeson
        && isJust (toObject aeson)
        && caseAesonObject Nothing (Just <<< fromObject) aeson == Just aeson

  test "BigNumber encoding/decoding and functions coherence" $
    quickCheckGen' 10000 do
      aeson <- aesonBigNumberGen defaultNumberGenConf
      pure $
        ( flip (caseAesonBigNumber false) aeson \bn ->
            BigNumber.isFinite bn && not (BigNumber.isNaN bn)
        )
          && isBigNumber aeson
          && isJust (toBigNumber aeson)
          && caseAesonFiniteBigNumber Nothing (Just <<< fromFiniteBigNumber) aeson == Just aeson

  test "Int encoding/decoding and functions coherence" $ do
    assertTrue_ $ (toInt <$> parseJsonStringToAeson "1.0") == Right (Just 1)
    assertTrue_ $ (toInt <$> parseJsonStringToAeson "1.0000000000000001") == Right (Nothing)

    quickCheckGen' 10000 do
      fbn <- finiteBigNumberGen defaultNumberGenConf
      let
        aeson = fromFiniteBigNumber fbn
        bn = unpackFinite fbn

      let
        intInBounds =
          BigNumber.isInteger bn
            && BigNumber.fromInt bottom <= bn
            && bn <= BigNumber.fromInt top

      let
        coherence =
          isInt aeson
            && isJust (toInt aeson)
            && caseAesonInt Nothing (Just <<< fromInt) aeson == Just aeson

      pure $ intInBounds `iff` coherence

  test "UInt encoding/decoding and functions coherence" $ do
    assertTrue_ $ (toUInt <$> parseJsonStringToAeson "1.0") == Right (Just (UInt.fromInt 1))

    assertTrue_ $ (toUInt <$> parseJsonStringToAeson "1.0000000000000001") == Right (Nothing)

    quickCheckGen' 10000 do
      fbn <- finiteBigNumberGen defaultNumberGenConf
      let
        aeson = fromFiniteBigNumber fbn
        bn = unpackFinite fbn

      let
        intInBounds =
          BigNumber.isInteger bn
            && BigNumber.fromUInt bottom <= bn
            && bn <= BigNumber.fromUInt top

      let
        coherence =
          isUInt aeson
            && isJust (toUInt aeson)
            && caseAesonUInt Nothing (Just <<< fromUInt) aeson == Just aeson

      pure $ intInBounds `iff` coherence

  test "BigInt encoding/decoding and functions coherence" $
    quickCheckGen' 10000 do
      fbn <- finiteBigNumberGen defaultNumberGenConf { expDigitsUpTo = 2 }
      let
        aeson = fromFiniteBigNumber fbn
        bn = unpackFinite fbn

      let
        coherence =
          isBigInt aeson
            && isJust (toBigInt aeson)
            && caseAesonBigInt Nothing (Just <<< fromBigInt) aeson == Just aeson

      pure $ BigNumber.isInteger bn `iff` coherence

  test "Number encoding/decoding and functions coherence" $
    quickCheckGen' 10000 do
      aeson <- aesonBigNumberGen defaultNumberGenConf

      let
        isFiniteNumber =
          caseAesonBigNumber false
            ( \bn ->
                let
                  n = BigNumber.toNumber bn
                in
                  Number.isFinite n && not (Number.isNaN n)
            )
            aeson

      pure $ isFiniteNumber `iff`
        ( isNumber aeson
            && isJust (toNumber aeson)
            && caseAesonFiniteNumber false (const true) aeson
        )
  -- use eqApproximate from Data.Number.Approximate here?
  -- && caseAesonFiniteNumber Nothing (Just <<< fromFiniteNumber) aeson == Just aeson

  test "Maybe encoding/decoding" $ do
    quickCheckGen' 10000 do
      aeson <- unsafePartial $ oneOf [ aesonNullGen, aesonGen ]
      pure $ isNull aeson `iff` (decodeAeson aeson == Right (Nothing :: Maybe Aeson))

  ----

  let
    tstStr = "{\"a\":10,\"c\":{\"d\":\"val\"},\"e\": null}"
    tstAeson = fromRight undefined $ parseJsonStringToAeson tstStr
    tstObj = unsafePartial $ fromJust $ toObject $ tstAeson
    tstRec = { a: 10, c: { d: "val" } }

  test "Record encoding/decoding" $ do

    assertTrue_ $ decodeJsonString tstStr == Right tstRec

    -- Fields in stringified record are sorted
    -- by RowToList, this is an implementation detail.
    -- Thus, we can not expect:
    -- assertTrue_ $ (stringifyAeson $ encodeAeson tstRec) == (tstStr)
    assertTrue_ $ (decodeJsonString $ stringifyAeson $ encodeAeson tstRec) == Right tstRec

  test "getField"
    $ assertTrue_
    $ getField tstObj "a" == Right 10

  test "getNestedAeson"
    $ assertTrue_
    $ (getNestedAeson tstAeson [ "c", "d" ] >>= decodeAeson) == Right "val"

  test "getFieldOptional'" $ do
    assertTrue_ $ (getFieldOptional' tstObj "x") == Right (Nothing :: Maybe Aeson)
    assertTrue_ $ (getFieldOptional' tstObj "e") == Right (Nothing :: Maybe Aeson)

  test "getFieldOptional" $ do
    assertTrue_ $ (getFieldOptional tstObj "x") == Right (Nothing :: Maybe Aeson)
    assertTrue_ $ (getFieldOptional tstObj "e") == Right (Just aesonNull :: Maybe Aeson)
    assertTrue_ $ isLeft (getFieldOptional tstObj "e" :: _ (Maybe Boolean))

  test "Tuple encoding/decoding" $ do
    let
      tuple4 :: Boolean /\ Boolean /\ Maybe Boolean /\ Array Boolean
      tuple4 = false /\ true /\ Nothing /\ []
      tuple4Str = "[false,true,null,[]]"

    assertTrue_ $ decodeJsonString tuple4Str == Right tuple4
    assertTrue_ $ stringifyAeson (encodeAeson tuple4) == tuple4Str

    assertTrue_ $
      (decodeJsonString "false" :: _ (Int /\ Int)) ==
        Left (TypeMismatch "Tuple")

    assertTrue_ $
      (decodeJsonString tuple4Str :: _ (Int /\ Int)) ==
        Left (TypeMismatch "Tuple2")

    assertTrue_ $
      (decodeJsonString tuple4Str :: _ (Int /\ Int /\ Int)) ==
        Left (TypeMismatch "Tuple3")

    assertTrue_ $
      (decodeJsonString tuple4Str :: _ (Boolean /\ Int /\ Int /\ Int)) ==
        Left (AtIndex 1 $ TypeMismatch "Int")

  test "Json -> Aeson" $ do
    quickCheckGen' 10000 do
      json <- jsonGen

      let
        -- alternative implementation of jsonToAeson
        -- if both implementations match, they are,
        -- probably, fine
        yetAnotherJsonToAeson :: Json -> Aeson
        yetAnotherJsonToAeson =
          unsafePartial alwaysRight <<< decodeJsonString <<< Argonaut.stringify
          where
          -- valid json should always decode without errors
          -- error "Impossible happened: valid json should always decode without errors"
          alwaysRight :: Partial => _
          alwaysRight (Right x) = x

      pure $ jsonToAeson json == yetAnotherJsonToAeson json

  -- At least, we don't expect any numers in Json we got from
  -- toStringifiedNumbersJson
  --
  -- Better test, which tests that strings, representing numbers,
  -- actually correspond to initial numbers
  -- requires to track number positions
  -- as in result numbers are no more distinguishable from other strings
  test "Aeson -> Json" $ do
    quickCheckGen' 10000 do
      aeson <- aesonGen

      let
        -- alternative implementation of jsonToAeson
        -- if both implementations match, they are,
        -- probably, fine
        noNumber :: Json -> Boolean
        noNumber = fix \self -> caseJson
          (const true)
          (const true)
          (const false) -- number
          (const true)
          (all self)
          (all self)

      pure $ noNumber $ toStringifiedNumbersJson aeson

  test "Map encode/decode" $ do
    quickCheckGen' 100 do
      _map <- Map.genMap (arbitrary :: _ Int) aesonGen

      pure $
        (decodeJsonString $ stringifyAeson $ encodeAeson _map) == Right _map

  test "Set encode/decode" $ do
    quickCheckGen' 100 do
      set <- Set.fromFoldable <$> sizedArray (arbitrary :: _ Int)

      pure $
        (decodeJsonString $ stringifyAeson $ encodeAeson set) == Right set


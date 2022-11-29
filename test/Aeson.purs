module Test.Aeson where

import Prelude

import Aeson
  ( Aeson
  , AesonCases
  , caseAeson
  , caseAesonBoolean
  , caseAesonNull
  , caseAesonString
  , constAesonCases
  , decodeAeson
  , decodeJsonString
  , encodeAeson
  , getField
  , getNestedAeson
  , jsonToAeson
  , parseJsonStringToAeson
  , stringifyAeson
  , toObject
  , class EncodeAeson
  , class DecodeAeson
  , JsonDecodeError(ParsingError)
  )
import Effect.Aff (Aff)
import Control.Apply (lift2)
import Data.Argonaut as Json
import Data.Array (head, zip, (!!))
import Data.BigNumber as BigNumber
import Data.Either (Either(Right, Left), hush)
import Data.Maybe (Maybe(Nothing, Just), fromJust, fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Foreign.Object as FO
import Mote (group, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, readdir)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import Test.ArbitraryJson (ArbBigInt(..), ArbJson, ArbUInt(..), stringifyArbJson)
import Test.QuickCheck (quickCheck', (<?>))
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (assertTrue)
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite = do
  group "Aeson encode" do
    test "Incorrect JSON" $ liftEffect do
      let error = Left ParsingError :: Either JsonDecodeError Int
      decodeJsonString "{" `shouldEqual` error

    test "Record" $ liftEffect do
      let
        expected = { a: 10 }
      decodeJsonString "{\"a\": 10}" `shouldEqual` Right expected
    test "Object" $ liftEffect do
      let
        expected = FO.fromFoldable [ "a" /\ 1, "b" /\ 2 ]
      decodeAeson (encodeAeson expected) `shouldEqual` Right expected
    test "Maybe" $ liftEffect do
      let
        expected = Just 1
      decodeAeson (encodeAeson expected) `shouldEqual` Right expected

  group "Object field accessing" do
    let
      asn = unsafePartial $ fromRight $ parseJsonStringToAeson
        "{\"a\": 10, \"b\":[{\"b1\":\"valb\"}], \"c\":{\"c1\": \"valc\"}}"
      asnObj = unsafePartial $ fromJust $ toObject $ asn
    test "getField" $ liftEffect do
      getField asnObj "a" `shouldEqual` Right 10
      getField asnObj "b" `shouldEqual` Right ([ { b1: "valb" } ])

    test "getNestedAeson" $ liftEffect do
      (getNestedAeson asn [ "c", "c1" ] >>= decodeAeson) `shouldEqual`
        (Right "valc")

  group "Json <-> Aeson" do
    test "jsonToAeson" $ liftEffect do
      let
        jsnStr =    "{\"a\":10,\"b\":[{\"b1\":\"valb\"}],\"c\":{\"c1\":\"valc\"}}"
        jsn = unsafePartial $ fromRight $ Json.parseJson jsnStr
      (jsonToAeson jsn # stringifyAeson) `shouldEqual` jsnStr

  group "caseAeson" do
    test "caseObject" $ liftEffect do
      let asn = encodeAeson { a: 10 }
      (caseMaybeAeson _ { caseObject = Just <<< flip getField "a" }) asn
        `shouldEqual`
          (Just $ Right 10)
    test "caseArray" $ liftEffect do
      let asn = encodeAeson [ 10 ]
      (caseMaybeAeson _ { caseArray = map decodeAeson <<< head }) asn
        `shouldEqual`
          (Just $ Right 10)
    test "caseNumber" $ liftEffect do
      let asn = encodeAeson 20222202
      (caseMaybeAeson _ { caseBigNumber = Just }) asn `shouldEqual`
        (hush $ BigNumber.parseBigNumber "20222202")

  group "Fixture tests for parseJsonStringifyNumbers" $ do
    test "parseNumbersTests" $ liftEffect parseNumbersTests
    test "parseStringTests" $ liftEffect parseStringTests
    test "parseBoolAndNullTests" $ liftEffect parseBoolAndNullTests
    test "fixtureTests" fixtureTests

  group "Arbitrary Aeson" do
    testStringifyArbitraryAeson

  group "EncodeAeson >>> DecodeAeson == identity" do
    testEncodeDecodeAesonIdentity

-- | This function reads from `./fixtures/` folder.
-- | `expected/*` contains JSONs corresponding to `Aeson` type (with number
-- | index) as returned by `parseJsonExtractingIntegers`.
-- | Here the number index in fixtures is compared with the parsed one.
fixtureTests :: Aff Unit
fixtureTests = do
  fixtures <- lift2 zip (readFixtures "input/") (readFixtures "expected/")
  for_ fixtures $ mkTest (uncurry testFixture)
  where
  testFixture
    :: Tuple FilePath String -> Tuple FilePath String -> Tuple String Boolean
  testFixture (Tuple inputPath input) (Tuple _expectedPath expected) =
    let
      mkError msg = Tuple (msg <> ": " <> show inputPath) false
    in
      case parseJsonStringToAeson input /\ Json.jsonParser expected of
        Right _aeson /\ Right json ->
          case Json.caseJsonArray Nothing (\arr -> arr !! 1) json of
            Nothing -> mkError "Failed to decode expected json"
            Just (res :: Json.Json) ->
              case Json.decodeJson res :: Either _ (Array String) of
                Left _ -> mkError "Unable to decode NumberIndex"
                Right _ -> pure true
        Left err /\ _ -> mkError $ "Failed to parse input JSON: " <> show err
        _ /\ Left err -> mkError $ "Failed to parse expected JSON: " <> show err

  readFixtures :: FilePath -> Aff (Array (Tuple FilePath String))
  readFixtures dirn =
    let
      d = (fixtureDir <> dirn)
      readTestFile fp = Tuple fp <$> readTextFile UTF8 fp
    in
      readdir d >>= traverse (readTestFile <<< (<>) d)

  fixtureDir = "./fixtures/test/parsing/json_stringify_numbers/"

-- | Make simple test
mkTest :: forall a. (a -> Tuple String Boolean) -> a -> Aff Unit
mkTest doTest inp =
  let
    Tuple errMsg testRes = doTest inp
  in
    if testRes then pure unit
    else liftEffect $ throwException $ error errMsg

parseBoolAndNullTests :: Effect Unit
parseBoolAndNullTests = do
  testNull
  testBoolean "false"
  testBoolean "true"
  where
  testNull = testSimpleValue "null" $ \json ->
    Tuple "jsonTurnNumbersToStrings altered null value"
      (caseAesonNull false (const true) json)

  testBoolean s = testSimpleValue s $ \json ->
    Tuple "jsonTurnNumbersToStrings altered null value"
      (caseAesonBoolean false (const true) json)

parseNumbersTests :: Effect Unit
parseNumbersTests = do
  testNumber "123123123123123123123100"
  testNumber "100"
  testNumber "0.2"
  --- "-10e-20" will not work,
  --- cuz we translate all numbers to non-exponent form
  where
  testNumber s = testSimpleValue s $ \aeson -> do
    Tuple
      ("parseJsonStringifyNumbers changed read number: " <> s <> " -> " <> stringifyAeson aeson)
      (stringifyAeson aeson == s)

parseStringTests :: Effect Unit
parseStringTests = do
  testString "\"\""
  testString "\"test\""
  testString "\"1231231\""
  testString "\"123\\\"1231\\\"12\""
  testString "\"sth\\\"12sd31\\\"s12\""
  where
  testString s = testSimpleValue s $ \aeson ->
    caseAesonString
      ( Tuple
          ( "parseJsonStringifyNumbers produced no string when parsing string: "
              <> stringifyAeson
                aeson
          )
          false
      )
      ( \decoded -> Tuple
          ( "parseJsonStringifyNumbers changed read string: " <> s <> " -> " <>
              decoded
          )
          (show decoded == s)
      )
      aeson

caseMaybeAeson
  :: forall b a
   . (AesonCases (Maybe a) -> AesonCases (Maybe b))
  -> Aeson
  -> Maybe b
caseMaybeAeson upd = caseAeson (constAesonCases Nothing # upd)

fromRight :: forall (a :: Type) (e :: Type). Partial => Either e a -> a
fromRight (Right x) = x

testSimpleValue :: String -> (Aeson -> Tuple String Boolean) -> Effect Unit
testSimpleValue s jsonCb = uncurry assertTrue $
  case parseJsonStringToAeson s of
    Left _ -> Tuple
      -- TODO doc
      ("Argonaut could not parse jsonTurnNumbersToStrings result: " <> s)
      false
    Right json -> jsonCb json

testStringifyArbitraryAeson :: TestPlanM Unit
testStringifyArbitraryAeson = liftEffect $ quickCheck' 3000 \arbJson ->
  let
    jsonString = stringifyArbJson arbJson
    res = do
      aeson1 <- hush $ parseJsonStringToAeson jsonString
      aeson2 <- hush $ parseJsonStringToAeson $ stringifyAeson aeson1
      pure $ aeson1 /\ aeson2
  in
    fromMaybe false (res <#> uncurry eq) <?>
      "Test failed for input " <> show (isJust res) <> " - " <> jsonString

testEncodeDecodeAesonIdentity :: TestPlanM Unit
testEncodeDecodeAesonIdentity = do
  test "Int" $ liftEffect $ quickCheck' 30 \(i :: Int) -> roundtrip i
  test "BigInt" $ liftEffect $ quickCheck' 30 \(ArbBigInt i) -> roundtrip i
  test "UInt" $ liftEffect $ quickCheck' 30 \(ArbUInt i) -> roundtrip i
  test "Boolean" $ liftEffect $ quickCheck' 3 \(i :: Boolean) -> roundtrip i
  test "String" $ liftEffect $ quickCheck' 30 \(i :: String) -> roundtrip i
  test "Tuple" $ liftEffect $ quickCheck' 30 \(i :: Tuple String Int) -> roundtrip i
  test "Number" $ liftEffect $ quickCheck' 30 \(i :: Number) -> roundtrip i
  test "Array" $ liftEffect $ quickCheck' 30 \(i :: Array ArbBigInt) ->
    (i # map unwrap # encodeAeson # decodeAeson) == Right (map unwrap i)
  test "Record" $ liftEffect $ quickCheck' 30
    \( i
         :: { a :: { b :: Int, c :: Number, d :: Array Int }
            , e :: { f :: String, g :: Boolean }
            }
     ) -> roundtrip i
  test "Aeson" $ liftEffect $ quickCheck' 100 \(i :: ArbJson) ->
    let j = i # arbAeson in (j # encodeAeson # decodeAeson) == Right j
  where
  arbAeson aj = unsafePartial $ fromJust $ hush $ parseJsonStringToAeson $
    stringifyArbJson aj
  roundtrip :: forall a . Eq a => EncodeAeson a => DecodeAeson a => a -> Boolean
  roundtrip i = (decodeAeson $ encodeAeson i) == Right i

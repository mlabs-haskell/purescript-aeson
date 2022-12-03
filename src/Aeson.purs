-- | Uses `json-bigint` library to parse JSON.
-- | Stores numbers as `BigNumber` from `bignumber.js`.
-- | API and behaviour is intended to be close to Aeson.

module Aeson (
  (.:)
  , (.:?)
  , Aeson
  , AesonCases
  , class EncodeAeson
  , class GEncodeAeson
  , class DecodeAeson
  , class DecodeAesonField
  , class GDecodeAeson
  , caseAeson

  , caseAesonArray
  , caseAesonBigInt
  , caseAesonBigNumber
  , caseAesonBoolean
  , caseAesonInt
  , caseAesonNull
  , caseAesonNumber
  , caseAesonObject
  , caseAesonString
  , caseAesonUInt

  , constAesonCases

  , decodeAeson
  , decodeAesonField
  , decodeJsonString
  , encodeAeson
  , gDecodeAeson
  , gEncodeAeson
  , getField
  , getFieldOptional
  , getFieldOptional'
  , getNestedAeson
  , jsonToAeson
  , parseJsonStringToAeson
  , stringifyAeson

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

  , toArray
  , toBigInt
  , toBigNumber
  , toBoolean
  , toInt
  , toNull
  , toNumber
  , toObject
  , toString
  , toUInt

  , fromArray
  , fromBigInt
  , fromBigNumber
  , fromBoolean
  , fromInt
  , fromNumber
  , fromObject
  , fromString
  , fromUInt

  , aesonNull
  , toStringifiedNumbersJson
  , module DataArgonautReexport
) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Argonaut (Json, JsonDecodeError(..))
import Data.Argonaut (JsonDecodeError(..), printJsonDecodeError) as DataArgonautReexport
import Data.Argonaut (fromArray, fromObject, jsonNull, stringify) as Argonaut
import Data.Argonaut.Encode.Encoders (encodeBoolean, encodeString)
import Data.Array (fromFoldable, toUnfoldable, (!!))
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.BigNumber (BigNumber)
import Data.BigNumber as BigNumber
import Data.Either (Either(Right, Left), fromRight, note)
import Data.Foldable (foldM, intercalate)
import Data.Int (decimal)
import Data.Int as Int
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Number (fromString) as Number
import Data.Number.Format (toString) as Number
import Data.Sequence (Seq)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Typelevel.Undefined (undefined)
import Data.UInt (UInt)
import Data.UInt as UInt
import Foreign.Object (Object)
import Foreign.Object as FO
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Prelude (Proxy(Proxy))
import Untagged.Union (class InOneOf, type (|+|), asOneOf)

-- | A piece of JSON where all numbers are represented as BigNumber (from bignumber.js)
foreign import data Aeson :: Type

foreign import aesonEq :: Aeson -> Aeson -> Boolean

instance Eq Aeson where
  eq = aesonEq

instance Show Aeson where
  show = stringifyAeson

class DecodeAeson (a :: Type) where
  decodeAeson :: Aeson -> Either JsonDecodeError a

-------- Parsing: String -> Aeson --------

foreign import parseAeson
  :: (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> String -> Maybe Aeson

parseJsonStringToAeson :: String -> Either JsonDecodeError Aeson
parseJsonStringToAeson payload =
  note (TypeMismatch "JSON String")
    $ parseAeson Nothing Just payload

-- -------- Stringifying: Aeson -> String

foreign import stringifyAeson :: Aeson -> String

-- -------- Json <-> Aeson --------

-- | Replaces numbers the Aeson's payload with stringified
-- | numbers
-- | Given original payload of: `{"a": 10}`
-- | The result will be an Json object representing: `{"a": "10"}`
toStringifiedNumbersJson :: Aeson -> Json
toStringifiedNumbersJson = fix \_ ->
  caseAeson
    { caseNull: const Argonaut.jsonNull
    , caseBoolean: encodeBoolean
    , caseBigNumber: encodeString <<< BigNumber.toFixed
    , caseString: encodeString
    , caseArray: map toStringifiedNumbersJson >>> Argonaut.fromArray
    , caseObject: map toStringifiedNumbersJson >>> Argonaut.fromObject
    }

-- | Recodes Argonaut Json to Aeson.
-- | NOTE. The operation is costly as its stringifies given Json
-- |       and reparses resulting string as Aeson.
jsonToAeson :: Json -> Aeson
jsonToAeson = Argonaut.stringify >>> decodeJsonString >>> fromRight shouldNotHappen
  where
  -- valid json should always decode without errors
  -- error "Impossible happened: valid json should always decode without errors"
  shouldNotHappen = undefined

-------- Aeson manipulation and field accessors --------

getField
  :: forall (a :: Type)
   . DecodeAeson a
  => Object Aeson
  -> String
  -> Either JsonDecodeError a
getField aesonObject field = getField' decodeAeson aesonObject field
  where
  -- | Adapted from `Data.Argonaut.Decode.Decoders`
  getField'
    :: (Aeson -> Either JsonDecodeError a)
    -> Object Aeson
    -> String
    -> Either JsonDecodeError a
  getField' decoder obj str =
    maybe
      (Left $ AtKey str MissingValue)
      (lmap (AtKey str) <<< decoder)
      (FO.lookup str obj)

infix 7 getField as .:

-- | Attempt to get the value for a given key on an `Object Aeson`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | but will fail if the key is present but the value cannot be converted to the right type.
-- |
-- | This function will treat `null` as a value and attempt to decode it into your desired type.
-- | If you would like to treat `null` values the same as absent values, use
-- | `getFieldOptional'` (`.:?`) instead.
getFieldOptional
  :: forall (a :: Type)
   . DecodeAeson a
  => Object Aeson
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional = getFieldOptional_ decodeAeson
  where
  getFieldOptional_
    :: (Aeson -> Either JsonDecodeError a)
    -> Object Aeson
    -> String
    -> Either JsonDecodeError (Maybe a)
  getFieldOptional_ decoder obj str =
    maybe (pure Nothing) (map Just <<< decode) (FO.lookup str obj)
    where
    decode = lmap (AtKey str) <<< decoder

infix 7 getFieldOptional as .:!

-- | Attempt to get the value for a given key on an `Object Aeson`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | or if the key is present and the value is `null`.
-- |
-- | Use this accessor if the key and value are optional in your object.
-- | If the key and value are mandatory, use `getField` (`.:`) instead.
getFieldOptional'
  :: forall (a :: Type)
   . DecodeAeson a
  => Object Aeson
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional' = getFieldOptional'_ decodeAeson
  where
  getFieldOptional'_
    :: (Aeson -> Either JsonDecodeError a)
    -> Object Aeson
    -> String
    -> Either JsonDecodeError (Maybe a)
  getFieldOptional'_ decoder obj str =
    maybe (pure Nothing) decode (FO.lookup str obj)
    where
    decode aeson =
      if isNull aeson then
        pure Nothing
      else
        Just <$> (lmap (AtKey str) <<< decoder) aeson

infix 7 getFieldOptional' as .:?

-- | Returns an Aeson available under a sequence of keys in given Aeson.
-- | If not possible returns JsonDecodeError.
getNestedAeson :: Aeson -> Array String -> Either JsonDecodeError Aeson
getNestedAeson aeson keys =
  note (TypeMismatch typeName) $
     foldM lookup aeson keys
  where
  lookup :: Aeson -> String -> Maybe Aeson
  lookup j lbl = caseAesonObject Nothing (FO.lookup lbl) j

  -- Given list of keys: ["foo", "bar", "baz"]
  -- Expected error message
  -- " Expected value of type Record.foo.bar.baz"
  typeName = "Record." <> intercalate "." keys

-- | Utility abbrevation. See `caseAeson` for an example usage.
type AesonCases a =
  { caseNull :: Unit -> a
  , caseBoolean :: Boolean -> a
  , caseBigNumber :: BigNumber -> a
  , caseString :: String -> a
  , caseArray :: Array Aeson -> a
  , caseObject :: Object Aeson -> a
  }

foreign import _caseAeson
  :: forall a
   . (Unit -> a)
  -> (Boolean -> a)
  -> (BigNumber -> a)
  -> (String -> a)
  -> (Array Aeson -> a)
  -> (Object Aeson -> a)
  -> Aeson -> a

caseAeson
  :: forall (a :: Type)
   . AesonCases a
  -> Aeson
  -> a
caseAeson
  { caseNull, caseBoolean, caseBigNumber, caseString, caseArray, caseObject }
  (aeson) =
    _caseAeson caseNull caseBoolean caseBigNumber caseString caseArray caseObject aeson

constAesonCases :: forall (a :: Type). a -> AesonCases a
constAesonCases v =
  { caseObject: c
  , caseNull: c
  , caseBoolean: c
  , caseString: c
  , caseBigNumber: c
  , caseArray: c
  }
  where
  c :: forall (b :: Type). b -> a
  c = const v


decodeNumber ∷ ∀ a. (String → Maybe a) -> Aeson -> Maybe a
decodeNumber fromString' =
    caseAesonBigNumber Nothing $
      fromString' <<< BigNumber.toFixed

caseAesonObject :: forall (a :: Type). a -> (Object Aeson -> a) -> Aeson -> a
caseAesonObject def f = caseAeson (constAesonCases def # _ { caseObject = f })

caseAesonString :: forall (a :: Type). a -> (String -> a) -> Aeson -> a
caseAesonString def f = caseAeson (constAesonCases def # _ { caseString = f })

caseAesonArray :: forall (a :: Type). a -> (Array Aeson -> a) -> Aeson -> a
caseAesonArray def f = caseAeson (constAesonCases def # _ { caseArray = f })

caseAesonBoolean :: forall (a :: Type). a -> (Boolean -> a) -> Aeson -> a
caseAesonBoolean def f = caseAeson (constAesonCases def # _ { caseBoolean = f })

-- | `caseAesonBigNumber` specialized to `Int` (fails if no parse)
caseAesonInt :: forall (a :: Type). a -> (Int -> a) -> Aeson -> a
caseAesonInt def f = maybe def f <<< decodeNumber (Int.fromStringAs decimal)

-- | `caseAesonBigNumber` specialized to `UInt` (fails if no parse)
caseAesonUInt :: forall (a :: Type). a -> (UInt -> a) -> Aeson -> a
caseAesonUInt def f = maybe def f <<< decodeNumber UInt.fromString

-- | `caseAesonBigNumber` specialized to `BigInt` (fails if no parse)
caseAesonBigInt :: forall (a :: Type). a -> (BigInt -> a) -> Aeson -> a
caseAesonBigInt def f = maybe def f <<< decodeNumber BigInt.fromString

-- | `caseAesonNumber` specialized to `BigNumber` (fails if no parse)
caseAesonBigNumber :: forall (a :: Type). a -> (BigNumber -> a) -> Aeson -> a
caseAesonBigNumber def f = caseAeson (constAesonCases def # _ { caseBigNumber = f })

-- | `caseAesonBigNumber` specialized to `Number` (fails if no parse)
caseAesonNumber :: forall (a :: Type). a -> (Number -> a) -> Aeson -> a
caseAesonNumber def f = maybe def f <<< decodeNumber Number.fromString

caseAesonNull :: forall (a :: Type). a -> (Unit -> a) -> Aeson -> a
caseAesonNull def f = caseAeson (constAesonCases def # _ { caseNull = f })

isAesonType :: forall a. (Boolean -> (a -> Boolean) -> Aeson -> Boolean) -> Aeson -> Boolean
isAesonType cs = cs false (const true)

-- | Check if the provided `Json` is the `null` value
isNull :: Aeson -> Boolean
isNull = isAesonType caseAesonNull

-- | Check if the provided `Aeson` is a `Boolean`
isBoolean :: Aeson -> Boolean
isBoolean = isAesonType caseAesonBoolean

-- | Check if the provided `Aeson` is a `Int`
isInt :: Aeson -> Boolean
isInt = isAesonType caseAesonInt

-- | Check if the provided `Aeson` is a `UInt`
isUInt :: Aeson -> Boolean
isUInt = isAesonType caseAesonUInt

-- | Check if the provided `Aeson` is a `BigInt`
isBigInt :: Aeson -> Boolean
isBigInt = isAesonType caseAesonBigInt

-- | Check if the provided `Aeson` is a `Number`
isNumber :: Aeson -> Boolean
isNumber = isAesonType caseAesonNumber

-- | Check if the provided `Aeson` is a `BigNumber`
isBigNumber :: Aeson -> Boolean
isBigNumber = isAesonType caseAesonBigNumber

-- | Check if the provided `Aeson` is a `String`
isString :: Aeson -> Boolean
isString = isAesonType caseAesonString

-- | Check if the provided `Aeson` is an `Array`
isArray :: Aeson -> Boolean
isArray = isAesonType caseAesonArray

-- | Check if the provided `Aeson` is an `Object`
isObject :: Aeson -> Boolean
isObject = isAesonType caseAesonObject

toAesonType
  :: forall a
  . (Maybe a -> (a -> Maybe a) -> Aeson -> Maybe a)
  -> Aeson
  -> Maybe a
toAesonType cs = cs Nothing Just

-- | Convert `Aeson` to the `Unit` value if the `Aeson` is the null value
toNull :: Aeson -> Maybe Unit
toNull = toAesonType caseAesonNull

-- | Convert `Aeson` to a `Boolean` value, if the `Aeson` is a boolean.
toBoolean :: Aeson -> Maybe Boolean
toBoolean = toAesonType caseAesonBoolean

-- | Convert `Aeson` to a `Int` value, if the `Aeson` can be treated as Int.
toInt :: Aeson -> Maybe Int
toInt = toAesonType caseAesonInt

-- | Convert `Aeson` to a `UInt` value, if the `Aeson` can be treated as UInt.
toUInt :: Aeson -> Maybe UInt
toUInt = toAesonType caseAesonUInt

-- | Convert `Aeson` to a `BigInt` value, if the `Aeson` can be treated as BigInt.
toBigInt :: Aeson -> Maybe BigInt
toBigInt = toAesonType caseAesonBigInt

-- | Convert `Aeson` to a `Number` value, if the `Aeson` can be treated as Number.
toNumber :: Aeson -> Maybe Number
toNumber = toAesonType caseAesonNumber

-- | Convert `Aeson` to a `BigNumber` value, if the `Aeson` is a BigNumber.
toBigNumber :: Aeson -> Maybe BigNumber
toBigNumber = toAesonType caseAesonBigNumber

-- | Convert `Aeson` to a `String` value, if the `Aeson` is a string. To write a
-- | `Aeson` value to a JSON string, see `stringify`.
toString :: Aeson -> Maybe String
toString = toAesonType caseAesonString

-- | Convert `Aeson` to an `Array` of `Aeson` values, if the `Aeson` is an array.
toArray :: Aeson -> Maybe (Array Aeson)
toArray = toAesonType caseAesonArray

-- | Convert `Aeson` to an `Object` of `Aeson` values, if the `Aeson` is an object.
toObject :: Aeson -> Maybe (Object Aeson)
toObject = toAesonType caseAesonObject

-------- Decode helpers --------

-- | Decodes a value encoded as JSON via Aeson decoding algorithm.
decodeJsonString
  :: forall (a :: Type). DecodeAeson a => String -> Either JsonDecodeError a
decodeJsonString = parseJsonStringToAeson >=> decodeAeson

-------- DecodeAeson instances --------

instance DecodeAeson UInt where
  decodeAeson = caseAesonUInt
    (Left $ TypeMismatch "UInt")
    Right

instance DecodeAeson Int where
  decodeAeson = caseAesonInt
    (Left $ TypeMismatch "Int")
    Right

instance DecodeAeson BigInt where
  decodeAeson = caseAesonBigInt
    (Left $ TypeMismatch "BigInt")
    Right

instance DecodeAeson Number where
  decodeAeson = caseAesonNumber
    (Left $ TypeMismatch "Number")
    Right

instance DecodeAeson BigNumber where
  decodeAeson = caseAesonBigNumber
    (Left $ TypeMismatch "BigNumber")
    Right

instance DecodeAeson Boolean where
  decodeAeson = caseAesonBoolean
    (Left $ TypeMismatch "Boolean")
    Right

instance DecodeAeson String where
  decodeAeson = caseAesonString
    (Left $ TypeMismatch "String")
    Right

instance DecodeAeson Aeson where
  decodeAeson = pure

instance DecodeAeson a => DecodeAeson (Object a) where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Object")
    (traverse decodeAeson)

instance (DecodeAeson a, DecodeAeson b) => DecodeAeson (Tuple a b) where
  decodeAeson = caseAesonArray (Left err) \arr ->
      case arr !! 0, arr !! 1, arr !! 2 of
        Just a, Just b, Nothing ->
          Tuple <$> decodeAeson a <*> decodeAeson b
        _, _, _ -> Left err
    where
      err = TypeMismatch "Tuple"

instance
  ( GDecodeAeson row list
  , RL.RowToList row list
  ) =>
  DecodeAeson (Record row) where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Record")
    \object -> gDecodeAeson object (Proxy :: Proxy list)

instance
  ( InOneOf b a b
  , DecodeAeson a
  , DecodeAeson b
  ) =>
  DecodeAeson (a |+| b) where
  decodeAeson j =
    asOneOf <$> (decodeAeson j :: Either JsonDecodeError a)
      <|> asOneOf <$> (decodeAeson j :: Either JsonDecodeError b)

instance DecodeAeson a => DecodeAeson (Array a) where
  decodeAeson = caseAesonArray
    (Left $ TypeMismatch "Array")
    (traverse decodeAeson)

instance DecodeAeson a => DecodeAeson (L.List a) where
  decodeAeson x = toUnfoldable <$> decodeAeson x

instance DecodeAeson a => DecodeAeson (LL.List a) where
  decodeAeson x = toUnfoldable <$> decodeAeson x

instance DecodeAeson a => DecodeAeson (Seq a) where
  decodeAeson x = toUnfoldable <$> decodeAeson x

instance DecodeAeson a => DecodeAeson (Maybe a) where
  decodeAeson aeson =
    caseAeson
    { caseNull: \_ -> Right Nothing
    , caseBoolean: \_ -> Just <$> decodeAeson aeson
    , caseBigNumber: \_ -> Just <$> decodeAeson aeson
    , caseString: \_ -> Just <$> decodeAeson aeson
    , caseArray: \_ -> Just <$> decodeAeson aeson
    , caseObject: \_ -> Just <$> decodeAeson aeson
    }
    aeson

class
  GDecodeAeson (row :: Row Type) (list :: RL.RowList Type)
  | list -> row where
  gDecodeAeson
    :: forall proxy
     . Object Aeson
    -> proxy list
    -> Either JsonDecodeError (Record row)

instance GDecodeAeson () RL.Nil where
  gDecodeAeson _ _ = Right {}

instance
  ( DecodeAesonField value
  , GDecodeAeson rowTail tail
  , IsSymbol field
  , Row.Cons field value rowTail row
  , Row.Lacks field rowTail
  ) =>
  GDecodeAeson row (RL.Cons field value tail) where
  gDecodeAeson object _ = do
    let
      _field = Proxy :: Proxy field
      fieldName = reflectSymbol _field
      fieldValue = FO.lookup fieldName object

    case decodeAesonField fieldValue of
      Just fieldVal -> do
        val <- lmap (AtKey fieldName) fieldVal
        rest <- gDecodeAeson object (Proxy :: Proxy tail)
        Right $ Record.insert _field val rest

      Nothing ->
        Left $ AtKey fieldName MissingValue

class DecodeAesonField a where
  decodeAesonField :: Maybe Aeson -> Maybe (Either JsonDecodeError a)

instance DecodeAeson a => DecodeAesonField (Maybe a) where
  decodeAesonField = Just <<< maybe (Right Nothing) decodeAeson

else instance DecodeAeson a => DecodeAesonField a where
  decodeAesonField j = decodeAeson <$> j

-------- EncodeAeson --------

-- | Construct the `Json` representation of a `*` value.
-- | Note that this function only produces `Json` containing a single piece of `*`
foreign import fromBoolean :: Boolean -> Aeson
-- | This function does NOT convert the `String` encoding of a JSON value to `Json`
foreign import fromString :: String -> Aeson
foreign import fromBigNumber :: BigNumber -> Aeson
foreign import fromArray :: Array Aeson -> Aeson
foreign import fromObject :: Object Aeson -> Aeson
foreign import aesonNull :: Aeson

fromInt ∷ Int → Aeson
fromInt = encodeAeson

fromUInt ∷ UInt → Aeson
fromUInt = encodeAeson

fromBigInt ∷ BigInt → Aeson
fromBigInt = encodeAeson

fromNumber ∷ Number → Aeson
fromNumber = encodeAeson

class EncodeAeson (a :: Type) where
  encodeAeson :: a -> Aeson

instance EncodeAeson Int where
  -- bigNumberToString should never fail on strings obtained by stringifying of an Int
  -- thus `fromRight undefined` is safe here
  encodeAeson = fromRight undefined <<< map encodeAeson
    <<< BigNumber.parseBigNumber <<< Int.toStringAs decimal

instance EncodeAeson BigInt where
  -- bigNumberToString should never fail on strings obtained by stringifying of a BigInt
  -- thus `fromRight undefined` is safe here
  encodeAeson = fromRight undefined <<< map encodeAeson
    <<< BigNumber.parseBigNumber <<< BigInt.toString

instance EncodeAeson UInt where
  encodeAeson = encodeAeson <<< UInt.toInt

instance EncodeAeson Number where
  -- bigNumberToString should never fail on strings obtained by stringifying of a Number
  -- thus `fromRight undefined` is safe here
  encodeAeson = fromRight undefined <<< map encodeAeson
    <<< BigNumber.parseBigNumber <<< Number.toString

instance EncodeAeson BigNumber where
  encodeAeson = fromBigNumber

instance EncodeAeson String where
  encodeAeson = fromString

instance EncodeAeson Boolean where
  encodeAeson = fromBoolean

instance EncodeAeson Aeson where
  encodeAeson = identity

instance EncodeAeson a => EncodeAeson (Object a) where
  encodeAeson input = fromObject $ map encodeAeson input

instance
  ( GEncodeAeson row list
  , RL.RowToList row list
  ) =>
  EncodeAeson (Record row) where
  encodeAeson rec = encodeAeson $ gEncodeAeson rec (Proxy :: Proxy list)

instance EncodeAeson a => EncodeAeson (Array a) where
  encodeAeson x = fromArray $ map encodeAeson x

instance (EncodeAeson a, EncodeAeson b) => EncodeAeson (Tuple a b) where
  -- We represent tuple as 2-element JS array
  encodeAeson (Tuple a b) = encodeAeson [ encodeAeson a, encodeAeson b ]

instance EncodeAeson a => EncodeAeson (L.List a) where
  encodeAeson = encodeAeson <<< fromFoldable

instance EncodeAeson a => EncodeAeson (LL.List a) where
  encodeAeson = encodeAeson <<< fromFoldable

instance EncodeAeson a => EncodeAeson (Seq a) where
  encodeAeson = encodeAeson <<< fromFoldable

instance EncodeAeson a => EncodeAeson (Maybe a) where
  encodeAeson Nothing = aesonNull
  encodeAeson (Just a) = encodeAeson a

class GEncodeAeson (row :: Row Type) (list :: RL.RowList Type) where
  gEncodeAeson
    :: forall proxy. Record row -> proxy list -> FO.Object Aeson

instance gEncodeAesonNil :: GEncodeAeson row RL.Nil where
  gEncodeAeson _ _ = FO.empty

instance gEncodeAesonCons ::
  ( EncodeAeson value
  , GEncodeAeson row tail
  , IsSymbol field
  , Row.Cons field value tail' row
  ) =>
  GEncodeAeson row (RL.Cons field value tail) where
  gEncodeAeson row _ = do
    let _field = Proxy :: Proxy field
    FO.insert
      (reflectSymbol _field)
      (encodeAeson $ Record.get _field row)
      (gEncodeAeson row (Proxy :: Proxy tail))

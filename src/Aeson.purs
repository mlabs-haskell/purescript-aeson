-- | Argonaut can't decode long integers the way Aeson encodes them: they
-- | lose precision on the stage of `JSON.parse` call, which we can't really
-- | control. This module is a hacky solution allowing us to preserve long
-- | integers while decoding.
-- | The idea is that we process JSON-as-string in FFI, exctracting all numbers
-- | into a separate array named "index", where they are represented as strings,
-- | and place that index alongside the original json. We modify the original
-- | JSON such that it contains indices of original numbers in the array,
-- | instead of the actual numbers.
-- |
-- | E.g. from `{ "a": 42, "b": 24 }` we get
-- | `{ json: {"a": 0, "b": 1 }, index: [ "42", "24" ] }`.
-- |
-- | Then, in decoders for `Int` and `BigInt` we access that array to get the
-- | values back
-- |
-- | Known limitations: does not support Record decoding (no GDecodeJson-like
-- | machinery). But it is possible to decode records manually, because
-- | `getField` is implemented.
module Aeson
  ( (.:)
  , (.:?)
  , Aeson
  , AesonCases
  , AesonEncoder
  , NumberIndex
  , class EncodeAeson
  , class EncodeAeson'
  , class GEncodeAeson
  , class DecodeAeson
  , class DecodeAesonField
  , class GDecodeAeson
  , bumpNumberIndexBy
  , caseAeson
  , caseAesonArray
  , caseAesonBigInt
  , caseAesonBoolean
  , caseAesonNull
  , caseAesonNumber
  , caseAesonObject
  , caseAesonString
  , caseAesonUInt
  , constAesonCases
  , decodeAeson
  , decodeAesonField
  , decodeAesonViaJson
  , decodeJsonString
  , encodeAeson
  , encodeAeson'
  , encodeAesonViaJson
  , gDecodeAeson
  , gEncodeAeson
  , useNextIndexIndex
  , getCurrentNumberIndex
  , getField
  , getFieldOptional
  , getFieldOptional'
  , getNestedAeson
  , getNumberIndex
  , jsonToAeson
  , parseJsonStringToAeson
  , stringifyAeson
  , toStringifiedNumbersJson
  , isNull
  , isBoolean
  , isNumber
  , isString
  , isArray
  , isObject
  , toNull
  , toBoolean
  , toNumber
  , toString
  , toArray
  , toObject
  , fromString
  , aesonNull
  , encodeTraversable
  , decodeTraversable
  , module X
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.RWS (modify_)
import Control.Monad.State (State, evalState, get)
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , Json
  , JsonDecodeError(MissingValue, AtKey, TypeMismatch, UnexpectedValue)
  , caseJson
  , caseJsonObject
  , decodeJson
  , encodeJson
  , fromArray
  , fromObject
  , jsonNull
  , stringify
  )
import Data.Argonaut
  ( JsonDecodeError
    ( TypeMismatch
    , UnexpectedValue
    , AtIndex
    , AtKey
    , Named
    , MissingValue
    )
  , printJsonDecodeError
  ) as X
import Data.Argonaut (isNull, fromString) as Argonaut
import Data.Argonaut.Encode.Encoders (encodeBoolean, encodeString, encodeUnit)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (foldr, fromFoldable, (!!))
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left), fromRight, note)
import Data.Foldable (fold, foldM)
import Data.Int (round)
import Data.Int as Int
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Number as Number
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (class Traversable, for, sequence, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Typelevel.Undefined (undefined)
import Data.UInt (UInt)
import Data.UInt as UInt
import Foreign.Object (Object)
import Foreign.Object as FO
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Prelude (Proxy(Proxy))
import Untagged.Union (class InOneOf, type (|+|), asOneOf)

-- | A piece of JSON where all numbers are replaced with their indexes
newtype AesonPatchedJson = AesonPatchedJson Json

-- | A piece of JSON where all numbers are extracted into `NumberIndex`.
newtype Aeson = Aeson
  { patchedJson :: AesonPatchedJson, numberIndex :: NumberIndex }

instance Eq Aeson where
  eq a b = stringifyAeson a == stringifyAeson b

instance Show Aeson where
  show = stringifyAeson

-- | A list of numbers extracted from Json, as they appear in the payload.
type NumberIndex = Seq String

class DecodeAeson (a :: Type) where
  decodeAeson :: Aeson -> Either JsonDecodeError a

-------- Parsing: String -> Aeson --------

foreign import parseJsonExtractingIntegers
  :: String
  -> { patchedPayload :: String, numberIndex :: Array String }

parseJsonStringToAeson :: String -> Either JsonDecodeError Aeson
parseJsonStringToAeson payload = do
  let { patchedPayload, numberIndex } = parseJsonExtractingIntegers payload
  patchedJson <- lmap (const MissingValue) $ AesonPatchedJson <$> jsonParser
    patchedPayload
  pure $ Aeson { numberIndex: Seq.fromFoldable numberIndex, patchedJson }

-------- Stringifying: Aeson -> String

foreign import stringifyAeson_ :: Array String -> AesonPatchedJson -> String

stringifyAeson :: Aeson -> String
stringifyAeson (Aeson { patchedJson, numberIndex }) = stringifyAeson_
  (fromFoldable numberIndex)
  patchedJson

-------- Json <-> Aeson --------

-- | Replaces indexes in the Aeson's payload with stringified
-- | numbers from numberIndex.
-- | Given original payload of: `{"a": 10}`
-- | The result will be an Json object representing: `{"a": "10"}`
toStringifiedNumbersJson :: Aeson -> Json
toStringifiedNumbersJson = fix \_ ->
  caseAeson
    { caseNull: const jsonNull
    , caseBoolean: encodeBoolean
    , caseNumber: encodeString
    , caseString: encodeString
    , caseArray: map toStringifiedNumbersJson >>> fromArray
    , caseObject: map toStringifiedNumbersJson >>> fromObject
    }

-- | Recodes Json to Aeson.
-- | NOTE. The operation is costly as its stringifies given Json
-- |       and reparses resulting string as Aeson.
jsonToAeson :: Json -> Aeson
jsonToAeson = stringify >>> decodeJsonString >>> fromRight shouldNotHappen
  where
  -- valid json should always decode without errors
  shouldNotHappen = undefined

getNumberIndex :: Aeson -> NumberIndex
getNumberIndex (Aeson { numberIndex }) = numberIndex

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
    decode aeson@(Aeson { patchedJson: AesonPatchedJson json }) =
      if Argonaut.isNull json then
        pure Nothing
      else
        Just <$> (lmap (AtKey str) <<< decoder) aeson

infix 7 getFieldOptional' as .:?

-- | Returns an Aeson available under a sequence of keys in given Aeson.
-- | If not possible returns JsonDecodeError.
getNestedAeson :: Aeson -> Array String -> Either JsonDecodeError Aeson
getNestedAeson
  asn@(Aeson { numberIndex, patchedJson: AesonPatchedJson pjson })
  keys =
  note (UnexpectedValue $ toStringifiedNumbersJson asn) $
    mkAeson <$> (foldM lookup pjson keys :: Maybe Json)
  where
  lookup :: Json -> String -> Maybe Json
  lookup j lbl = caseJsonObject Nothing (FO.lookup lbl) j

  mkAeson :: Json -> Aeson
  mkAeson json = Aeson { numberIndex, patchedJson: AesonPatchedJson json }

-- | Utility abbrevation. See `caseAeson` for an example usage.
type AesonCases a =
  { caseNull :: Unit -> a
  , caseBoolean :: Boolean -> a
  , caseNumber :: String -> a
  , caseString :: String -> a
  , caseArray :: Array Aeson -> a
  , caseObject :: Object Aeson -> a
  }

caseAeson
  :: forall (a :: Type)
   . AesonCases a
  -> Aeson
  -> a
caseAeson
  { caseNull, caseBoolean, caseNumber, caseString, caseArray, caseObject }
  (Aeson { numberIndex, patchedJson: AesonPatchedJson pJson }) = caseJson
  caseNull
  caseBoolean
  (coerceNumber >>> unsafeSeqIndex numberIndex >>> caseNumber)
  caseString
  (map mkAeson >>> caseArray)
  (map mkAeson >>> caseObject)
  pJson
  where
  mkAeson :: Json -> Aeson
  mkAeson json = Aeson { patchedJson: AesonPatchedJson json, numberIndex }

  -- will never get index out of bounds
  unsafeSeqIndex :: forall b. Seq b -> Int -> b
  unsafeSeqIndex s ix = unsafePartial $ fromJust $ Seq.index ix s

  -- will never encounter non int number
  coerceNumber :: Number -> Int
  coerceNumber = round

constAesonCases :: forall (a :: Type). a -> AesonCases a
constAesonCases v =
  { caseObject: c
  , caseNull: c
  , caseBoolean: c
  , caseString: c
  , caseNumber: c
  , caseArray: c
  }
  where
  c :: forall (b :: Type). b -> a
  c = const v

caseAesonObject :: forall (a :: Type). a -> (Object Aeson -> a) -> Aeson -> a
caseAesonObject def f = caseAeson (constAesonCases def # _ { caseObject = f })

caseAesonString :: forall (a :: Type). a -> (String -> a) -> Aeson -> a
caseAesonString def f = caseAeson (constAesonCases def # _ { caseString = f })

caseAesonArray :: forall (a :: Type). a -> (Array Aeson -> a) -> Aeson -> a
caseAesonArray def f = caseAeson (constAesonCases def # _ { caseArray = f })

caseAesonBoolean :: forall (a :: Type). a -> (Boolean -> a) -> Aeson -> a
caseAesonBoolean def f = caseAeson (constAesonCases def # _ { caseBoolean = f })

-- | String representation is used to allow users to choose numeric representation downstream.
caseAesonNumber :: forall (a :: Type). a -> (String -> a) -> Aeson -> a
caseAesonNumber def f = caseAeson (constAesonCases def # _ { caseNumber = f })

-- | `caseAesonNumber` specialized to `UInt` (fails if no parse)
caseAesonUInt :: forall (a :: Type). a -> (UInt -> a) -> Aeson -> a
caseAesonUInt def f = caseAesonNumber def \str ->
  case UInt.fromString str of
    Nothing -> def
    Just res -> f res

-- | `caseAesonNumber` specialized to `BigInt` (fails if no parse)
caseAesonBigInt :: forall (a :: Type). a -> (BigInt -> a) -> Aeson -> a
caseAesonBigInt def f = caseAesonNumber def \str ->
  case BigInt.fromString str of
    Nothing -> def
    Just res -> f res

caseAesonNull :: forall (a :: Type). a -> (Unit -> a) -> Aeson -> a
caseAesonNull def f = caseAeson (constAesonCases def # _ { caseNull = f })

verbAesonType :: forall a b. b -> (a -> b) -> (b -> (a -> b) -> Aeson -> b) -> Aeson -> b
verbAesonType def f g = g def f

isAesonType :: forall a. (Boolean -> (a -> Boolean) -> Aeson -> Boolean) -> Aeson -> Boolean
isAesonType = verbAesonType false (const true)

-- | Check if the provided `Json` is the `null` value
isNull :: Aeson -> Boolean
isNull = isAesonType caseAesonNull

-- | Check if the provided `Aeson` is a `Boolean`
isBoolean :: Aeson -> Boolean
isBoolean = isAesonType caseAesonBoolean

-- | Check if the provided `Aeson` is a `Number`
isNumber :: Aeson -> Boolean
isNumber = isAesonType caseAesonNumber

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
toAesonType = verbAesonType Nothing Just

-- | Convert `Aeson` to the `Unit` value if the `Aeson` is the null value
toNull :: Aeson -> Maybe Unit
toNull = toAesonType caseAesonNull

-- | Convert `Aeson` to a `Boolean` value, if the `Aeson` is a boolean.
toBoolean :: Aeson -> Maybe Boolean
toBoolean = toAesonType caseAesonBoolean

-- | Convert `Aeson` to a `Number` value, if the `Aeson` is a number.
toNumber :: Aeson -> Maybe String
toNumber = toAesonType caseAesonNumber

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

-- | Construct the `Json` representation of a `String` value.
-- | Note that this function only produces `Json` containing a single piece of `String`
-- | data (similar to `fromBoolean`, `fromNumber`, etc.).
-- | This function does NOT convert the `String` encoding of a JSON value to `Json` - For that
-- | purpose, you'll need to use `jsonParser`.
fromString :: String -> Aeson
fromString str = Aeson
  { patchedJson: AesonPatchedJson (Argonaut.fromString str), numberIndex: mempty }

aesonNull :: Aeson
aesonNull = Aeson
  { patchedJson: AesonPatchedJson jsonNull, numberIndex: mempty }

-------- Decode helpers --------

-- | Ignore numeric index and reuse Argonaut decoder.
decodeAesonViaJson
  :: forall (a :: Type). DecodeJson a => Aeson -> Either JsonDecodeError a
decodeAesonViaJson (Aeson { patchedJson: AesonPatchedJson j }) = decodeJson j

-- | Decodes a value encoded as JSON via Aeson decoding algorithm.
decodeJsonString
  :: forall (a :: Type). DecodeAeson a => String -> Either JsonDecodeError a
decodeJsonString = parseJsonStringToAeson >=> decodeAeson

-------- DecodeAeson instances --------

decodeNumber
  :: forall a. (String -> Maybe a) -> Aeson -> Either JsonDecodeError a
decodeNumber parse aeson@(Aeson { numberIndex }) = do
  -- Numbers are replaced by their index in the array.
  ix <- decodeAesonViaJson aeson
  numberStr <- note MissingValue (Seq.index ix numberIndex)
  note (TypeMismatch $ "Couldn't parse to integral: " <> numberStr)
    (parse numberStr)

instance DecodeAeson UInt where
  decodeAeson = decodeNumber UInt.fromString

instance DecodeAeson Int where
  decodeAeson = decodeNumber Int.fromString

instance DecodeAeson BigInt where
  decodeAeson = decodeNumber BigInt.fromString

instance DecodeAeson Boolean where
  decodeAeson = decodeAesonViaJson

instance DecodeAeson String where
  decodeAeson = decodeAesonViaJson

instance DecodeAeson Number where
  decodeAeson = decodeNumber Number.fromString

instance DecodeAeson Aeson where
  decodeAeson = pure

instance DecodeAeson a => DecodeAeson (Object a) where
  decodeAeson = caseAesonObject (Left (TypeMismatch "Expected Object"))
    (traverse decodeAeson)

instance (DecodeAeson a, DecodeAeson b) => DecodeAeson (Tuple a b) where
  decodeAeson = caseAesonArray (Left (TypeMismatch "Expected Array (Tuple)"))
    \arr ->
      case arr !! 0, arr !! 1, arr !! 2 of
        Just a, Just b, Nothing ->
          Tuple <$> decodeAeson a <*> decodeAeson b
        _, _, _ ->
          Left (TypeMismatch "Expected Array with length 2")

instance
  ( GDecodeAeson row list
  , RL.RowToList row list
  ) =>
  DecodeAeson (Record row) where
  decodeAeson json =
    case toObject json of
      Just object -> gDecodeAeson object (Proxy :: Proxy list)
      Nothing -> Left $ TypeMismatch "Object"

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
  decodeAeson = decodeTraversable

instance DecodeAeson a => DecodeAeson (L.List a) where
  decodeAeson = decodeTraversable

instance DecodeAeson a => DecodeAeson (LL.List a) where
  decodeAeson = map L.toUnfoldable <<< decodeTraversable

instance DecodeAeson a => DecodeAeson (Seq a) where
  decodeAeson = map L.toUnfoldable <<< decodeTraversable

instance DecodeAeson a => DecodeAeson (Maybe a) where
  decodeAeson aeson =
    caseAeson
    { caseNull: \_ -> Right Nothing
    , caseBoolean: \_ -> Just <$> decodeAeson aeson
    , caseNumber: \_ -> Just <$> decodeAeson aeson
    , caseString: \_ -> Just <$> decodeAeson aeson
    , caseArray: \_ -> Just <$> decodeAeson aeson
    , caseObject: \_ -> Just <$> decodeAeson aeson
    }
    aeson

decodeTraversable
  :: forall t a
  .  Traversable t
  => DecodeAeson a
  => DecodeJson (t Json)
  => Aeson
  -> Either JsonDecodeError (t a)
decodeTraversable (Aeson { numberIndex, patchedJson: AesonPatchedJson pJson }) = do
    jsons :: t Json <- decodeJson pJson
    for jsons \patchedJson -> do
      decodeAeson (Aeson { patchedJson: AesonPatchedJson patchedJson, numberIndex })

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

class EncodeAeson (a :: Type) where
  encodeAeson :: a -> Aeson

class EncodeAeson' (a :: Type) where
  encodeAeson' :: a -> AesonEncoder Aeson

runEncoder :: forall a. AesonEncoder a -> a
runEncoder (AesonEncoder s) = evalState s 0

newtype AesonEncoder a = AesonEncoder (State Int a)

derive newtype instance Functor AesonEncoder
derive newtype instance Apply AesonEncoder
derive newtype instance Applicative AesonEncoder
derive newtype instance Bind AesonEncoder
derive newtype instance Monad AesonEncoder

useNextIndexIndex :: AesonEncoder Int
useNextIndexIndex = AesonEncoder (get <* modify_ ((+) 1))

getCurrentNumberIndex :: AesonEncoder Int
getCurrentNumberIndex = AesonEncoder get

bumpNumberIndexBy :: Int -> AesonEncoder Unit
bumpNumberIndexBy i = AesonEncoder (modify_ ((+) i))

encodeAesonViaJson :: forall a. EncodeJson a => a -> AesonEncoder Aeson
encodeAesonViaJson v = pure $ Aeson
  { patchedJson: AesonPatchedJson $ encodeJson v, numberIndex: Seq.empty }

instance EncodeAeson' Int where
  encodeAeson' i = do
    ix <- useNextIndexIndex
    pure $ Aeson
      { patchedJson: AesonPatchedJson $ encodeJson ix
      , numberIndex: Seq.singleton (show i)
      }

else instance EncodeAeson' BigInt where
  encodeAeson' i = do
    ix <- useNextIndexIndex
    pure $ Aeson
      { patchedJson: AesonPatchedJson $ encodeJson ix
      , numberIndex: Seq.singleton (BigInt.toString i)
      }

else instance EncodeAeson' UInt where
  encodeAeson' i = do
    ix <- useNextIndexIndex
    pure $ Aeson
      { patchedJson: AesonPatchedJson $ encodeJson ix
      , numberIndex: Seq.singleton (UInt.toString i)
      }

else instance EncodeAeson' Number where
  encodeAeson' i = do
    ix <- useNextIndexIndex
    pure $ Aeson
      { patchedJson: AesonPatchedJson $ encodeJson ix
      , numberIndex: Seq.singleton (show i)
      }

else instance EncodeAeson' String where
  encodeAeson' = encodeAesonViaJson

else instance EncodeAeson' Boolean where
  encodeAeson' = encodeAesonViaJson

else instance EncodeAeson' Aeson where
  encodeAeson' (Aeson { patchedJson: AesonPatchedJson json, numberIndex }) = do
    ix <- getCurrentNumberIndex
    let
      bumpIndices = fix $ \_ -> caseJson encodeUnit encodeBoolean encodeNumber
        encodeString
        (fromArray <<< map bumpIndices)
        (fromObject <<< map bumpIndices)
      encodeNumber n = encodeJson $ Int.toNumber ix + n
    bumpNumberIndexBy (Seq.length numberIndex)
    pure $
      (Aeson { patchedJson: AesonPatchedJson (bumpIndices json), numberIndex })

else instance EncodeAeson' a => EncodeAeson' (Object a) where
  encodeAeson' input = do
    Tuple obj indices <-
      foldr step (Tuple FO.empty Seq.empty) <<< FO.toUnfoldable <$>
      sequence (encodeAeson' <$> input)
    pure $ Aeson
      { patchedJson: AesonPatchedJson (fromObject obj)
      , numberIndex: fold indices
      }
    where
    step
      :: Tuple String Aeson
      -> Tuple (Object Json) (Seq (Seq String))
      -> Tuple (Object Json) (Seq (Seq String))
    step
      (Tuple k (Aeson { patchedJson: AesonPatchedJson json, numberIndex }))
      (Tuple obj indices) =
      Tuple (FO.insert k json obj) (Seq.cons numberIndex indices)

else instance
  ( GEncodeAeson row list
  , RL.RowToList row list
  ) =>
  EncodeAeson' (Record row) where
  encodeAeson' rec = do
    Tuple obj indices <-
      foldr step (Tuple FO.empty Seq.empty) <<< FO.toUnfoldable <$>
        (sequence $ gEncodeAeson rec (Proxy :: Proxy list))
    pure $ Aeson
      { patchedJson: AesonPatchedJson (fromObject obj)
      , numberIndex: fold indices
      }
    where
    step
      :: Tuple String Aeson
      -> Tuple (Object Json) (Seq (Seq String))
      -> Tuple (Object Json) (Seq (Seq String))
    step
      (Tuple k (Aeson { patchedJson: AesonPatchedJson json, numberIndex }))
      (Tuple obj indices) =
      Tuple (FO.insert k json obj) (Seq.cons numberIndex indices)

else instance (EncodeAeson a, EncodeAeson b) => EncodeAeson' (Tuple a b) where
  encodeAeson' (Tuple a b) = encodeTraversable' [ encodeAeson a, encodeAeson b ]

else instance EncodeAeson' a => EncodeAeson' (Array a) where
  encodeAeson' = encodeTraversable'

else instance EncodeAeson' a => EncodeAeson' (L.List a) where
  encodeAeson' = encodeTraversable'

else instance EncodeAeson' a => EncodeAeson' (LL.List a) where
  encodeAeson' = encodeTraversable'

else instance EncodeAeson' a => EncodeAeson' (Seq a) where
  encodeAeson' = encodeTraversable'

else instance EncodeAeson' a => EncodeAeson' (Maybe a) where
  encodeAeson' Nothing = pure aesonNull
  encodeAeson' (Just a) = encodeAeson' a

else instance EncodeAeson a => EncodeAeson' a where
  encodeAeson' = encodeAeson' <<< encodeAeson

encodeTraversable
  :: forall (t :: Type -> Type) (a :: Type)
  .  Traversable t
  => EncodeAeson a
  => t a -> Aeson
encodeTraversable = runEncoder <<< encodeTraversable'

encodeTraversable'
  :: forall (t :: Type -> Type) (a :: Type)
  .  Traversable t
  => EncodeAeson' a
  => t a -> AesonEncoder Aeson
encodeTraversable' arr = do
    Tuple jsonArr indices <- foldM step (Tuple Seq.empty Seq.empty) arr
    pure $ Aeson
      { patchedJson: AesonPatchedJson (fromArray $ fromFoldable jsonArr)
      , numberIndex: fold indices
      }
    where
    step
      :: Tuple (Seq Json) (Seq (Seq String))
      -> a
      -> AesonEncoder (Tuple (Seq Json) (Seq (Seq String)))
    step (Tuple arrJson indices) a = do
      Aeson { patchedJson: AesonPatchedJson json, numberIndex } <- encodeAeson'
        a
      pure $ Tuple (Seq.snoc arrJson json) (Seq.snoc indices numberIndex)

class GEncodeAeson (row :: Row Type) (list :: RL.RowList Type) where
  gEncodeAeson
    :: forall proxy. Record row -> proxy list -> FO.Object (AesonEncoder Aeson)

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
      (encodeAeson' $ Record.get _field row)
      (gEncodeAeson row (Proxy :: Proxy tail))

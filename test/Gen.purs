module Test.Gen where

import Prelude

import Aeson (Aeson, aesonNull, fromArray, fromBigInt, fromBoolean, fromObject, fromString)
import Control.Lazy (defer)
import Data.Argonaut (Json)
import Data.Argonaut.Gen (genJson)
import Data.Array.NonEmpty as LNA
import Data.Int as Int
import Data.Maybe (fromJust, fromMaybe)
import Data.String as S
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Foreign.Object as FO
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, frequency, resize, sized, vectorOf)

frequency' :: Partial => forall a. Array (Tuple Number (Gen a)) -> Gen a
frequency' = frequency <<< fromJust <<< LNA.fromFoldable

oneOf :: forall a. Partial => Array (Gen a) -> Gen a
oneOf arr = frequency' $ Tuple 1.0 <$> arr

sizedArray :: forall a. Gen a -> Gen (Array a)
sizedArray gen = sized \size -> do
  len <- chooseInt 0 size
  if (len == 0) then pure []
  else vectorOf len $ resize (size / len) gen

aesonGen :: Gen Aeson
aesonGen = defer \_ -> unsafePartial frequency'
  [ (1.0 /\ aesonNullGen)
  , (2.0 /\ aesonBooleanGen)
  , (3.0 /\ aesonStringGen)
  , (4.0 /\ aesonBigIntGen defaultNumberGenConf)
  , (4.0 /\ aesonArrayGen aesonGen)
  , (4.0 /\ aesonObjectGen (Tuple <$> arbitrary <*> aesonGen))
  ]

aesonNullGen :: Gen Aeson
aesonNullGen = pure aesonNull

aesonBooleanGen :: Gen Aeson
aesonBooleanGen = fromBoolean <$> arbitrary

aesonStringGen :: Gen Aeson
aesonStringGen = fromString <$> arbitrary

aesonArrayGen :: Gen Aeson -> Gen Aeson
aesonArrayGen contentGen = fromArray <$> sizedArray contentGen

aesonObjectGen :: Gen (Tuple String Aeson) -> Gen Aeson
aesonObjectGen contentGen = fromObject <<< FO.fromFoldable <$>
  sizedArray contentGen

type NumberGenConf =
  { intDigitsUpTo :: Int
  }

defaultNumberGenConf :: NumberGenConf
defaultNumberGenConf =
  { intDigitsUpTo: 100
  }

-- Generates number string of form
-- [-]<digits>
bigIntStrGen :: NumberGenConf -> Gen String
bigIntStrGen { intDigitsUpTo } = do
  let
    -- Returns "integer string", of length from 1 up to size
    -- if size = 0 returns "0"
    intStr f t = Int.toStringAs Int.decimal <$> chooseInt f t

    largeIntStr 0 = intStr 0 9
    largeIntStr size =
      go =<< chooseInt 1 size
      where
      go _size = do
        i <- intStr 0 top
        let lenI = S.length i
        if lenI >= _size then pure $ S.take _size i
        else map (i <> _) $ go (_size - lenI)
  negSign <- arbitrary
  intPart <- largeIntStr intDigitsUpTo
  let
    number = when negSign "-" <> intPart
      where
      when true x = x
      when false _ = ""
  pure number

----

bigIntGen :: NumberGenConf -> Gen BigInt
bigIntGen cfg =
  (fromMaybe (BigInt.fromInt 0) <<< BigInt.fromString) <$> bigIntStrGen cfg

aesonBigIntGen :: NumberGenConf -> Gen Aeson
aesonBigIntGen cfg = fromBigInt <$> bigIntGen cfg

----

jsonGen :: Gen Json
jsonGen = genJson

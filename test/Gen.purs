module Test.Gen where

import Prelude

import Aeson (Aeson, Finite, aesonNull, finiteBigNumber, fromArray, fromBoolean, fromFiniteBigNumber, fromObject, fromString)
import Control.Lazy (defer, fix)
import Data.Argonaut (Json)
import Data.Argonaut.Gen (genJson)
import Data.BigNumber (BigNumber)
import Data.BigNumber as BigNumber
import Data.Either (fromRight)
import Data.Int as Int
import Data.List.NonEmpty as LNE
import Data.Maybe (fromJust, maybe)
import Data.String as S
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Foreign.Object as FO
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, frequency, resize, sized, vectorOf)

frequency' :: Partial => forall a. Array (Tuple Number (Gen a)) -> Gen a
frequency' = frequency <<< fromJust <<< LNE.fromFoldable

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
  , (4.0 /\ aesonBigNumberGen defaultNumberGenConf)
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
  , fracDigitsUpTo :: Int
  , expDigitsUpTo :: Int
  }

defaultNumberGenConf :: NumberGenConf
defaultNumberGenConf =
  { intDigitsUpTo : 100
  , fracDigitsUpTo : 100
  , expDigitsUpTo : 10
  }

-- Generates number string of form
-- [-]<digits>[.<digits>][+/-/][e<digits>]
bigNumberStrGen :: NumberGenConf ->Gen String
bigNumberStrGen { intDigitsUpTo, fracDigitsUpTo, expDigitsUpTo } = do
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
  -- forceSign <- arbitrary

  withFractional <- arbitrary

  withExp <- arbitrary
  negExpSign <- arbitrary
  forceExpSign <- arbitrary

  intPart <- largeIntStr intDigitsUpTo
  fracPart <- largeIntStr fracDigitsUpTo
  expPart <- largeIntStr expDigitsUpTo

  let
    number = when negSign "-" -- sign

      <> intPart
      <> when withFractional ("." <> fracPart)
      <> when withExp ("e" <> expSign <> expPart)
      where
      -- sign = if negSign then "-" else when forceSign "+"
      expSign = if negExpSign then "-" else when forceExpSign "+"
      when true x = x
      when false _ = ""

  pure number

----

bigNumberGen :: NumberGenConf -> Gen BigNumber
bigNumberGen cfg =
  (fromRight undefined <<< BigNumber.parseBigNumber) <$> bigNumberStrGen cfg

finiteBigNumberGen :: NumberGenConf -> Gen (Finite BigNumber)
finiteBigNumberGen cfg = fix \self ->
  maybe self pure <<< finiteBigNumber =<< bigNumberGen cfg

aesonBigNumberGen :: NumberGenConf -> Gen Aeson
aesonBigNumberGen cfg = fromFiniteBigNumber <$> finiteBigNumberGen cfg

----

jsonGen :: Gen Json
jsonGen = genJson

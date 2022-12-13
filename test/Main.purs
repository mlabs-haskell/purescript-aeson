module Test.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Aeson (suite)
import Test.TestM (interpret)

main :: Effect Unit
main = do
  launchAff_ $ interpret suite

module Test.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Aeson (suite)
import Test.Utils as Utils

main :: Effect Unit
main = do
  launchAff_ $ Utils.interpret suite

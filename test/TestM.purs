module Test.TestM where

import Prelude

import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff)
import Data.Foldable (sequence_)
import Effect.Class (liftEffect)
import Mote (MoteT, Plan, foldPlan, planT)
import Test.Spec (Spec, describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)


type TestPlanM a = MoteT (Const Void) (Effect Unit) Aff a

-- | We use `mote` here so that we can use effects to build up a test tree, which
-- | is then interpreted here in a pure context, mainly due to some painful types
-- | in Test.Spec which prohibit effects.
interpret :: TestPlanM Unit -> Aff Unit
interpret spif = do
  plan <- planT spif
  runSpec [ consoleReporter ] $ go plan
  where
  go :: Plan (Const Void) (Effect Unit) -> Spec Unit
  go =
    foldPlan
      (\x -> it x.label $ liftEffect $ x.value)
      (const $ pure unit)
      (\x -> describe x.label $ go x.value)
      sequence_

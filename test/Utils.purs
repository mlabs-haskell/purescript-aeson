module Test.Utils
  ( assertTrue
  , assertTrue_
  , errMaybe
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (error)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throwException, throw)


-- | Test a boolean value, throwing the provided string as an error if `false`
assertTrue
  :: forall (m :: Type -> Type)
   . Applicative m
  => MonadEffect m
  => String
  -> Boolean
  -> m Unit
assertTrue msg b = unless b $ liftEffect $ throwException $ error msg

assertTrue_
  :: forall (m :: Type -> Type)
   . Applicative m
  => MonadEffect m
  => Boolean
  -> m Unit
assertTrue_ = assertTrue "Boolean test failed"

errMaybe
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadEffect m
  => String
  -> Maybe a
  -> m a
errMaybe msg = case _ of
  Nothing -> liftEffect $ throw msg
  Just res -> pure res

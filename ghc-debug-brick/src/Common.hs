{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Common where

import qualified Brick.Types as T
import Lens.Micro
import Namespace

type Handler e s =
     T.BrickEvent Name e
  -> T.EventM Name s ()

-- | liftHandler lifts a handler which only operates on its own state into
-- a larger state. It won't work if the handler needs to modify something
-- from a larger scope.
liftHandler
  :: ASetter s s a a -- The mode to modify
  -> c        -- Inner state
  -> (c -> a) -- How to inject the new state
  -> Handler e c -- Handler for inner state
  -> Handler e s
liftHandler l c i h ev = do
  T.zoom (lens (const c) (\ s c' -> set l (i c') s)) (h ev)

-- Missing instance from brick
deriving instance Functor (T.BrickEvent n)

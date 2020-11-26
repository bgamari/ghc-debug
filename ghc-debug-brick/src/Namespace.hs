{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}

module Namespace where

data Name
  = Setup_KnownDebuggeesList
  | Connected_Paused_SavedClosuresList
  deriving (Eq, Ord, Show)
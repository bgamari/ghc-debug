{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}

module Namespace where

data Name
  = Setup_KnownDebuggeesList
  | Setup_KnownSnapshotsList
  | Connected_Paused_ClosureDetails
  | Connected_Paused_ClosureTree
  | CommandPicker_List
  | Overlay
  | Footer
  deriving (Eq, Ord, Show)

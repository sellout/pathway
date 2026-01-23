{-# LANGUAGE Safe #-}

module Data.Path.Type (module Type) where

import "pathway-internal" Data.Path.Internal.Type as Type
  ( Type (Any, Dir, File),
  )

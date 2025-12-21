{-# LANGUAGE Safe #-}

module Data.Path.Any
  ( Path,
  )
where

import "base" Data.Functor.Compose (Compose)
import "base" Data.Kind qualified as Kind
import "pathway-internal" Data.Path.Internal qualified as Path
import "this" Data.Path.Anchored (Anchored)
import "this" Data.Path.Functor (Flip, Flip1)
import "this" Data.Path.Unambiguous (Unambiguous)

-- | A path where neither the `Relativity` nor `Type` is tracked in the type
--   (but can be checked at runtime).
type Path :: Kind.Type -> Kind.Type
type Path rep = Anchored (Compose (Compose Unambiguous (Flip1 Flip rep)) Path.Path)

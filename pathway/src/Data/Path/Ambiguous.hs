{-# LANGUAGE Safe #-}

module Data.Path.Ambiguous
  ( Path (..),
  )
where

import "base" Data.Kind qualified as Kind
import "base" GHC.Generics (Generic, Generic1)
import "pathway-internal" Data.Path.Internal (List, Parents, Relativity)

type Path :: Relativity -> Kind.Type -> Kind.Type
data Path rel rep = Path
  { parents :: Parents rel,
    directories :: List rep,
    component :: rep
  }
  deriving stock (Generic, Generic1)

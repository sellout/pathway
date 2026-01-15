{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2025 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Path.Functor
  ( DFunctor,
    dmap,
    DTraversable,
    dtraverse,
    dtraverseMap,
    Flip (Flip),
    Flip1 (Flip1),
    unflip,
    unflip1,
  )
where

import "base" Control.Category ((.))
import "base" Data.Eq (Eq)
import "base" Data.Functor.Identity (Identity (Identity), runIdentity)
import "base" Data.Kind qualified as Kind
import "base" Data.Ord (Ord)
import "base" GHC.Generics (Generic)
import "base" Text.Read (Read)
import "base" Text.Show (Show)

-- |
--
--  __TODO__: Generalize the type of the one in Yaya.
type DFunctor ::
  (j -> j -> Kind.Type) ->
  (k -> k -> Kind.Type) ->
  ((i -> j) -> k) ->
  Kind.Constraint
class DFunctor jCat kCat d where
  dmap :: (forall a. f a `jCat` g a) -> d f `kCat` d g

type DTraversable ::
  (k -> k -> Kind.Type) ->
  ((j -> k) -> k) ->
  (k -> k) ->
  Kind.Constraint
class (DFunctor kCat kCat d) => DTraversable kCat d m where
  dtraverse :: (forall a. f a `kCat` m (g a)) -> d f `kCat` m (d g)

-- | A default `dmap` implementation for the most common `DTraversable`
--   instances.
dtraverseMap ::
  (DTraversable (->) d Identity) => (forall a. f a -> g a) -> d f -> d g
dtraverseMap f = runIdentity . dtraverse (Identity . f)

-- | __FIXME__: I think this is in base already
type Flip :: (a -> b -> Kind.Type) -> b -> a -> Kind.Type
newtype Flip f b a = Flip {unflip :: f a b}
  deriving stock (Eq, Generic, Ord, Read, Show)

type Flip1 :: (a -> b -> c -> Kind.Type) -> b -> a -> c -> Kind.Type
newtype Flip1 f b a c = Flip1 {unflip1 :: f a b c}
  deriving stock (Eq, Generic, Ord, Read, Show)

{-# LANGUAGE Safe #-}

module Data.Path.Functor
  ( DFunctor,
    dmap,
    Flip (Flip),
    Flip1 (Flip1),
    unflip,
    unflip1,
  )
where

import "base" Data.Eq (Eq)
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

-- | __FIXME__: I think this is in base already
type Flip :: (a -> b -> Kind.Type) -> b -> a -> Kind.Type
newtype Flip f b a = Flip {unflip :: f a b}
  deriving stock (Eq, Generic, Ord, Read, Show)

type Flip1 :: (a -> b -> c -> Kind.Type) -> b -> a -> c -> Kind.Type
newtype Flip1 f b a c = Flip1 {unflip1 :: f a b c}
  deriving stock (Eq, Generic, Ord, Read, Show)

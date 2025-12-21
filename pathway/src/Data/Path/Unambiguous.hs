{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested `Path.Type` constraints.
{-# LANGUAGE UndecidableInstances #-}

module Data.Path.Unambiguous
  ( Unambiguous (Directory, File),
    Path,
    partition,
    partitionPaths,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bifunctor (bimap, first, second)
import "base" Data.Eq (Eq)
import "base" Data.Foldable (foldr)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Kind qualified as Kind
import "base" Data.Ord (Ord)
import "base" GHC.Generics (Generic)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "pathway-internal" Data.Path.Internal qualified as Path
import "this" Data.Path.Functor (DFunctor, Flip, dmap, unflip)

-- | Disjunction of a type parameterized over `Type`.
--
--  __NB__: This type may be awkward to use directly, so see `UnambiguousPath`
--          for the common case.
type Unambiguous :: (Path.Type -> Kind.Type) -> Kind.Type
data Unambiguous f
  = Directory (f 'Path.Dir)
  | File (f 'Path.File)
  deriving stock (Generic)

deriving stock instance (Eq (f 'Path.Dir), Eq (f 'Path.File)) => Eq (Unambiguous f)

deriving stock instance (Ord (f 'Path.Dir), Ord (f 'Path.File)) => Ord (Unambiguous f)

deriving stock instance (Read (f 'Path.Dir), Read (f 'Path.File)) => Read (Unambiguous f)

deriving stock instance (Show (f 'Path.Dir), Show (f 'Path.File)) => Show (Unambiguous f)

instance DFunctor (->) (->) Unambiguous where
  dmap f = \case
    Directory dir -> Directory $ f dir
    File file -> File $ f file

partition :: [Unambiguous f] -> ([f 'Path.Dir], [f 'Path.File])
partition =
  foldr
    ( \case
        Directory dir -> first (dir :)
        File file -> second (file :)
    )
    ([], [])

-- | A path where the `Type` is not tracked in the type (but can be checked at
--   runtime).
type Path :: Path.Relativity -> Kind.Type -> Kind.Type
type Path rel rep = Unambiguous (Flip (Path.Path rel) rep)

partitionPaths ::
  [Path rel rep] ->
  ([Path.Path rel 'Path.Dir rep], [Path.Path rel 'Path.File rep])
partitionPaths = bimap (unflip <$>) (unflip <$>) . partition

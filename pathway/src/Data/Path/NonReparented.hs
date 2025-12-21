{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested `Path.Type` constraints.
{-# LANGUAGE UndecidableInstances #-}

module Data.Path.NonReparented
  ( NonReparented (Absolute, Relative),
    Path,
    partition,
    partitionPaths,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bifunctor (bimap, first, second)
import "base" Data.Bool (Bool (False))
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
import "this" Data.Path.Functor (DFunctor, Flip, Flip1, dmap, unflip, unflip1)

-- | Disjunction of a type parameterized over `Relativity`.
--
--  __NB__: This type may be awkward to use directly, so see `NonReparentedPath` for
--          the common case.
type NonReparented :: (Path.Relativity -> Kind.Type) -> Kind.Type
data NonReparented f
  = Absolute (f 'Path.Abs)
  | Relative (f ('Path.Rel 'False))
  deriving stock (Generic)

deriving stock instance (Eq (f 'Path.Abs), Eq (f ('Path.Rel 'False))) => Eq (NonReparented f)

deriving stock instance (Ord (f 'Path.Abs), Ord (f ('Path.Rel 'False))) => Ord (NonReparented f)

deriving stock instance (Read (f 'Path.Abs), Read (f ('Path.Rel 'False))) => Read (NonReparented f)

deriving stock instance (Show (f 'Path.Abs), Show (f ('Path.Rel 'False))) => Show (NonReparented f)

instance DFunctor (->) (->) NonReparented where
  dmap f = \case
    Absolute abs -> Absolute $ f abs
    Relative rel -> Relative $ f rel

partition :: [NonReparented f] -> ([f 'Path.Abs], [f ('Path.Rel 'False)])
partition =
  foldr
    ( \case
        Absolute abs -> first (abs :)
        Relative rel -> second (rel :)
    )
    ([], [])

-- | A path where the `Type` is not tracked in the type (but can be checked at
--   runtime).
type Path :: Path.Type -> Kind.Type -> Kind.Type
type Path typ rep = NonReparented (Flip (Flip1 Path.Path typ) rep)

partitionPaths ::
  [Path typ rep] -> ([Path.Path 'Path.Abs typ rep], [Path.Path ('Path.Rel 'False) typ rep])
partitionPaths = bimap (unflip1 . unflip <$>) (unflip1 . unflip <$>) . partition

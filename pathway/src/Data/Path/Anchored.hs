{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested `Path.Type` constraints.
{-# LANGUAGE UndecidableInstances #-}

module Data.Path.Anchored
  ( Anchored (Absolute, Relative, Reparented),
    Path,
    partition,
    partitionPaths,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Eq (Eq)
import "base" Data.Foldable (foldr)
import "base" Data.Function (flip, ($))
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
--  __NB__: This type may be awkward to use directly, so see `AnchoredPath` for
--          the common case.
type Anchored :: (Path.Relativity -> Kind.Type) -> Kind.Type
data Anchored f
  = Absolute (f 'Path.Abs)
  | Relative (f ('Path.Rel 'False))
  | Reparented (f ('Path.Rel 'True))
  deriving stock (Generic)

deriving stock instance
  (Eq (f 'Path.Abs), Eq (f ('Path.Rel 'False)), Eq (f ('Path.Rel 'True))) =>
  Eq (Anchored f)

deriving stock instance
  (Ord (f 'Path.Abs), Ord (f ('Path.Rel 'False)), Ord (f ('Path.Rel 'True))) =>
  Ord (Anchored f)

deriving stock instance
  (Read (f 'Path.Abs), Read (f ('Path.Rel 'False)), Read (f ('Path.Rel 'True))) =>
  Read (Anchored f)

deriving stock instance
  (Show (f 'Path.Abs), Show (f ('Path.Rel 'False)), Show (f ('Path.Rel 'True))) =>
  Show (Anchored f)

instance DFunctor (->) (->) Anchored where
  dmap f = \case
    Absolute abs -> Absolute $ f abs
    Relative rel -> Relative $ f rel
    Reparented rep -> Reparented $ f rep

partition :: [Anchored f] -> ([f 'Path.Abs], [f ('Path.Rel 'False)], [f ('Path.Rel 'True)])
partition =
  foldr
    ( flip \(f, s, t) -> \case
        Absolute abs -> (abs : f, s, t)
        Relative rel -> (f, rel : s, t)
        Reparented rep -> (f, s, rep : t)
    )
    ([], [], [])

-- | A path where the `Relativity` is not tracked in the type (but can be
--   checked at runtime).
type Path :: Path.Type -> Kind.Type -> Kind.Type
type Path typ rep = Anchored (Flip (Flip1 Path.Path typ) rep)

partitionPaths ::
  [Path typ rep] ->
  ( [Path.Path 'Path.Abs typ rep],
    [Path.Path ('Path.Rel 'False) typ rep],
    [Path.Path ('Path.Rel 'True) typ rep]
  )
partitionPaths =
  ( \(f, s, t) ->
      (unflip1 . unflip <$> f, unflip1 . unflip <$> s, unflip1 . unflip <$> t)
  )
    . partition

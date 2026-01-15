{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested `Path.Type` constraints.
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Path.NonReparented
  ( NonReparented (Absolute, Relative),
    nonReparented,
    partition,
    weaken,
    Path,
    mapPath,
    path,
    partitionPaths,
    weakenPath,
    AmbiguousPath,
    mapAmbiguousPath,
    ambiguousPath,
    partitionAmbiguousPaths,
    weakenAmbiguousPath,
  )
where

import "base" Control.Applicative (Alternative, empty, pure)
import "base" Control.Category ((.))
import "base" Data.Bifunctor (bimap, first, second)
import "base" Data.Bool (Bool (False, True))
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable, foldr)
import "base" Data.Function (($))
import "base" Data.Functor (Functor, fmap, (<$>))
import "base" Data.Functor.Compose (Compose (Compose), getCompose)
import "base" Data.Kind qualified as Kind
import "base" Data.Ord (Ord)
import "base" Data.Semigroup (Semigroup, (<>))
import "base" GHC.Generics (Generic)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "pathway-internal" Data.Path.Internal ((</>), (</?>))
import "pathway-internal" Data.Path.Internal qualified as Path
import "this" Data.Path.Ambiguous qualified as Ambiguous
import "this" Data.Path.Anchored (Anchored)
import "this" Data.Path.Anchored qualified as Anchored
import "this" Data.Path.Functor
  ( DFunctor,
    DTraversable,
    Flip (Flip),
    Flip1 (Flip1),
    dmap,
    dtraverse,
    unflip,
    unflip1,
  )

-- | Disjunction of a type parameterized over `Relativity`.
--
--  __NB__: This type may be awkward to use directly, so see `NonReparentedPath`
--          for the common case.
type NonReparented :: (Path.Relativity -> Kind.Type) -> Kind.Type
data NonReparented f
  = Absolute (f 'Path.Abs)
  | Relative (f ('Path.Rel 'False))
  deriving stock (Generic)

deriving stock instance
  (Eq (f 'Path.Abs), Eq (f ('Path.Rel 'False))) => Eq (NonReparented f)

deriving stock instance
  (Ord (f 'Path.Abs), Ord (f ('Path.Rel 'False))) => Ord (NonReparented f)

deriving stock instance
  (Read (f 'Path.Abs), Read (f ('Path.Rel 'False))) => Read (NonReparented f)

deriving stock instance
  (Show (f 'Path.Abs), Show (f ('Path.Rel 'False))) => Show (NonReparented f)

-- | Eliminator for `nonReparented`. Takes a function for handling each branch.
--   This is just a potentially shorter version of explicit pattern matching.
nonReparented ::
  (f 'Path.Abs -> a) ->
  (f ('Path.Rel 'False) -> a) ->
  NonReparented f ->
  a
nonReparented absFn relFn = \case
  Absolute abs -> absFn abs
  Relative rel -> relFn rel

instance DFunctor (->) (->) NonReparented where
  dmap f = nonReparented (Absolute . f) (Relative . f)

instance (Functor m) => DTraversable (->) NonReparented m where
  dtraverse f = nonReparented (fmap Absolute . f) (fmap Relative . f)

partition ::
  ( Foldable g,
    Alternative h,
    Semigroup (h (f 'Path.Abs)),
    Semigroup (h (f ('Path.Rel 'False)))
  ) =>
  g (NonReparented f) ->
  (h (f 'Path.Abs), h (f ('Path.Rel 'False)))
partition =
  foldr
    (nonReparented (first . (<>) . pure) (second . (<>) . pure))
    (empty, empty)

weaken :: NonReparented f -> Anchored f
weaken = nonReparented Anchored.Absolute Anchored.Relative

-- | A path where the `Type` is not tracked in the type (but can be checked at
--   runtime).
type Path :: Path.Type -> Kind.Type -> Kind.Type
type Path typ = Compose NonReparented (Flip (Flip1 Path.Path typ))

mapPath ::
  (forall rel. Path.Path rel typ rep -> Path.Path rel typ' rep') ->
  Path typ rep ->
  Path typ' rep'
mapPath f = Compose . dmap (Flip . Flip1 . f . unflip1 . unflip) . getCompose

instance
  Path.TotalOps
    (Path 'Path.Dir rep)
    (Path.Path ('Path.Rel 'False) typ rep)
    (Path typ rep)
  where
  parent </> child = mapPath (</> child) parent

instance
  Path.PartialOps
    (Path 'Path.Dir rep)
    (Path.Path ('Path.Rel 'True) typ rep)
    (Anchored.Path typ rep)
  where
  (</?>) = (</?>) . weakenPath

path ::
  (Path.Path 'Path.Abs typ rep -> a) ->
  (Path.Path ('Path.Rel 'False) typ rep -> a) ->
  Path typ rep ->
  a
path absFn relFn =
  nonReparented (absFn . unflip1 . unflip) (relFn . unflip1 . unflip)
    . getCompose

partitionPaths ::
  ( Foldable g,
    Functor g,
    Alternative h,
    Semigroup (h (Flip (Flip1 Path.Path typ) rep 'Path.Abs)),
    Semigroup (h (Flip (Flip1 Path.Path typ) rep ('Path.Rel 'False)))
  ) =>
  g (Path typ rep) ->
  (h (Path.Path 'Path.Abs typ rep), h (Path.Path ('Path.Rel 'False) typ rep))
partitionPaths =
  bimap (unflip1 . unflip <$>) (unflip1 . unflip <$>)
    . partition
    . fmap getCompose

weakenPath :: Path typ rep -> Anchored.Path typ rep
weakenPath = Compose . weaken . getCompose

type AmbiguousPath :: Kind.Type -> Kind.Type
type AmbiguousPath = Compose NonReparented (Flip Ambiguous.Path)

mapAmbiguousPath ::
  (forall rel. Ambiguous.Path rel rep -> Ambiguous.Path rel rep') ->
  AmbiguousPath rep ->
  AmbiguousPath rep'
mapAmbiguousPath f = Compose . dmap (Flip . f . unflip) . getCompose

instance
  Path.TotalOps
    (Path 'Path.Dir rep)
    (Ambiguous.Path ('Path.Rel 'False) rep)
    (AmbiguousPath rep)
  where
  parent </> child =
    Compose . dmap (\(Flip (Flip1 par)) -> Flip $ par </> child) $
      getCompose parent

-- | We don’t know if the parent path is absolute, so we have to assume
--   concatenating a reparented path onto it could fail.
instance
  Path.PartialOps
    (Path 'Path.Dir rep)
    (Ambiguous.Path ('Path.Rel 'True) rep)
    (Anchored.AmbiguousPath rep)
  where
  (</?>) = (</?>) . weakenPath

ambiguousPath ::
  (Ambiguous.Path 'Path.Abs rep -> a) ->
  (Ambiguous.Path ('Path.Rel 'False) rep -> a) ->
  AmbiguousPath rep ->
  a
ambiguousPath absFn relFn =
  nonReparented (absFn . unflip) (relFn . unflip) . getCompose

partitionAmbiguousPaths ::
  ( Foldable g,
    Functor g,
    Alternative h,
    Semigroup (h (Flip Ambiguous.Path rep 'Path.Abs)),
    Semigroup (h (Flip Ambiguous.Path rep ('Path.Rel 'False)))
  ) =>
  g (AmbiguousPath rep) ->
  (h (Ambiguous.Path 'Path.Abs rep), h (Ambiguous.Path ('Path.Rel 'False) rep))
partitionAmbiguousPaths =
  bimap (unflip <$>) (unflip <$>) . partition . fmap getCompose

weakenAmbiguousPath :: AmbiguousPath rep -> Anchored.AmbiguousPath rep
weakenAmbiguousPath = Compose . weaken . getCompose

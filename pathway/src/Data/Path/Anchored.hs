{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested `Path.Type` constraints.
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: 2025 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Path.Anchored
  ( Anchored (Absolute, Relative, Reparented),
    anchored,
    partition,
    Path,
    mapPath,
    traversePath,
    path,
    partitionPaths,
    AmbiguousPath,
    mapAmbiguousPath,
    ambiguousPath,
    partitionAmbiguousPaths,
  )
where

import "base" Control.Applicative (Alternative, empty, pure)
import "base" Control.Category ((.))
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

-- | Eliminator for `Anchored`. Takes a function for handling each branch.
--   This is just a potentially shorter version of explicit pattern matching.
anchored ::
  (f 'Path.Abs -> a) ->
  (f ('Path.Rel 'False) -> a) ->
  (f ('Path.Rel 'True) -> a) ->
  Anchored f ->
  a
anchored absFn relFn repFn = \case
  Absolute abs -> absFn abs
  Relative rel -> relFn rel
  Reparented rep -> repFn rep

instance DFunctor (->) (->) Anchored where
  dmap f = anchored (Absolute . f) (Relative . f) (Reparented . f)

instance (Functor m) => DTraversable (->) Anchored m where
  dtraverse f =
    anchored (fmap Absolute . f) (fmap Relative . f) (fmap Reparented . f)

partition ::
  ( Foldable g,
    Alternative h,
    Semigroup (h (f 'Path.Abs)),
    Semigroup (h (f ('Path.Rel 'False))),
    Semigroup (h (f ('Path.Rel 'True)))
  ) =>
  g (Anchored f) ->
  (h (f 'Path.Abs), h (f ('Path.Rel 'False)), h (f ('Path.Rel 'True)))
partition =
  foldr
    ( anchored
        (\abs (f, s, t) -> (pure abs <> f, s, t))
        (\rel (f, s, t) -> (f, pure rel <> s, t))
        (\rep (f, s, t) -> (f, s, pure rep <> t))
    )
    (empty, empty, empty)

-- | A path where the `Relativity` is not tracked in the type (but can be
--   checked at runtime).
type Path :: Path.Type -> Kind.Type -> Kind.Type
type Path typ = Compose Anchored (Flip (Flip1 Path.Path typ))

mapPath ::
  (forall rel. Path.Path rel typ rep -> Path.Path rel typ' rep') ->
  Path typ rep ->
  Path typ' rep'
mapPath f = Compose . dmap (Flip . Flip1 . f . unflip1 . unflip) . getCompose

traversePath ::
  (Functor m) =>
  (forall rel. Path.Path rel typ rep -> m (Path.Path rel typ' rep')) ->
  Path typ rep ->
  m (Path typ' rep')
traversePath f =
  fmap Compose
    . dtraverse (fmap (Flip . Flip1) . f . unflip1 . unflip)
    . getCompose

instance
  Path.TotalOps
    (Path 'Path.Dir rep)
    (Path.Path ('Path.Rel 'False) typ rep)
    (Path typ rep)
  where
  parent </> child = mapPath (</> child) parent

-- | We don’t know if the parent path is absolute, so we have to assume
--   concatenating a reparented path onto it could fail.
instance
  Path.PartialOps
    (Path 'Path.Dir rep)
    (Path.Path ('Path.Rel 'True) typ rep)
    (Path typ rep)
  where
  parent </?> child =
    fmap Compose $
      path
        (fmap (Absolute . Flip . Flip1) . (</?> child))
        (pure . Reparented . Flip . Flip1 . (</> child))
        (pure . Reparented . Flip . Flip1 . (</> child))
        parent

path ::
  (Path.Path 'Path.Abs typ rep -> a) ->
  (Path.Path ('Path.Rel 'False) typ rep -> a) ->
  (Path.Path ('Path.Rel 'True) typ rep -> a) ->
  Path typ rep ->
  a
path absFn relFn repFn =
  anchored
    (absFn . unflip1 . unflip)
    (relFn . unflip1 . unflip)
    (repFn . unflip1 . unflip)
    . getCompose

partitionPaths ::
  ( Foldable g,
    Functor g,
    Alternative h,
    Semigroup (h (Flip (Flip1 Path.Path typ) rep 'Path.Abs)),
    Semigroup (h (Flip (Flip1 Path.Path typ) rep ('Path.Rel 'False))),
    Semigroup (h (Flip (Flip1 Path.Path typ) rep ('Path.Rel 'True)))
  ) =>
  g (Path typ rep) ->
  ( h (Path.Path 'Path.Abs typ rep),
    h (Path.Path ('Path.Rel 'False) typ rep),
    h (Path.Path ('Path.Rel 'True) typ rep)
  )
partitionPaths =
  ( \(f, s, t) ->
      (unflip1 . unflip <$> f, unflip1 . unflip <$> s, unflip1 . unflip <$> t)
  )
    . partition
    . fmap getCompose

type AmbiguousPath :: Kind.Type -> Kind.Type
type AmbiguousPath = Compose Anchored (Flip Ambiguous.Path)

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
    (AmbiguousPath rep)
  where
  parent </?> child =
    fmap Compose $
      path
        (fmap (Absolute . Flip) . (</?> child))
        (pure . Reparented . Flip . (</> child))
        (pure . Reparented . Flip . (</> child))
        parent

ambiguousPath ::
  (Ambiguous.Path 'Path.Abs rep -> a) ->
  (Ambiguous.Path ('Path.Rel 'False) rep -> a) ->
  (Ambiguous.Path ('Path.Rel 'True) rep -> a) ->
  AmbiguousPath rep ->
  a
ambiguousPath absFn relFn repFn =
  anchored (absFn . unflip) (relFn . unflip) (repFn . unflip) . getCompose

partitionAmbiguousPaths ::
  ( Foldable g,
    Functor g,
    Alternative h,
    Semigroup (h (Flip Ambiguous.Path rep 'Path.Abs)),
    Semigroup (h (Flip Ambiguous.Path rep ('Path.Rel 'False))),
    Semigroup (h (Flip Ambiguous.Path rep ('Path.Rel 'True)))
  ) =>
  g (AmbiguousPath rep) ->
  ( h (Ambiguous.Path 'Path.Abs rep),
    h (Ambiguous.Path ('Path.Rel 'False) rep),
    h (Ambiguous.Path ('Path.Rel 'True) rep)
  )
partitionAmbiguousPaths =
  (\(f, s, t) -> (unflip <$> f, unflip <$> s, unflip <$> t))
    . partition
    . fmap getCompose

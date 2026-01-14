{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested `Path.Type` constraints.
{-# LANGUAGE UndecidableInstances #-}

module Data.Path.Unambiguous
  ( Unambiguous (Directory, File),
    unambiguous,
    partition,
    Path,
    mapPath,
    path,
    partitionPaths,
  )
where

import "base" Control.Applicative (Alternative, empty, pure)
import "base" Control.Category ((.))
import "base" Data.Bifunctor (bimap, first, second)
import "base" Data.Bool (Bool (False, True))
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable, foldr)
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
import "this" Data.Path.Functor
  ( DFunctor,
    DTraversable,
    Flip (Flip),
    dmap,
    dtraverse,
    unflip,
  )

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

-- | Eliminator for `Unambiguous`. Takes a function for handling each branch.
--   This is just a potentially shorter version of explicit pattern matching.
unambiguous :: (f 'Path.Dir -> a) -> (f 'Path.File -> a) -> Unambiguous f -> a
unambiguous dirFn fileFn = \case
  Directory dir -> dirFn dir
  File file -> fileFn file

instance DFunctor (->) (->) Unambiguous where
  dmap f = unambiguous (Directory . f) (File . f)

instance (Functor m) => DTraversable (->) Unambiguous m where
  dtraverse f = unambiguous (fmap Directory . f) (fmap File . f)

partition ::
  ( Foldable g,
    Alternative h,
    Semigroup (h (f 'Path.Dir)),
    Semigroup (h (f 'Path.File))
  ) =>
  g (Unambiguous f) ->
  (h (f 'Path.Dir), h (f 'Path.File))
partition =
  foldr
    (unambiguous (first . (<>) . pure) (second . (<>) . pure))
    (empty, empty)

-- | A path where the `Type` is not tracked in the type (but can be checked at
--   runtime).
type Path :: Path.Relativity -> Kind.Type -> Kind.Type
type Path rel = Compose Unambiguous (Flip (Path.Path rel))

mapPath ::
  (forall typ. Path.Path rel typ rep -> Path.Path rel' typ rep') ->
  Path rel rep ->
  Path rel' rep'
mapPath f = Compose . dmap (Flip . f . unflip) . getCompose

traversePath ::
  (Functor m) =>
  (forall typ. Path.Path rel typ rep -> m (Path.Path rel' typ rep')) ->
  Path rel rep ->
  m (Path rel' rep')
traversePath f = fmap Compose . dtraverse (fmap Flip . f . unflip) . getCompose

instance
  Path.TotalOps
    (Path.Path rel 'Path.Dir rep)
    (Path ('Path.Rel 'False) rep)
    (Path rel rep)
  where
  parent </> child = mapPath (parent </>) child

instance
  Path.TotalOps
    (Path.Path ('Path.Rel 'True) 'Path.Dir rep)
    (Path ('Path.Rel 'True) rep)
    (Path ('Path.Rel 'True) rep)
  where
  parent </> child = mapPath (parent </>) child

instance
  Path.TotalOps
    (Path.Path ('Path.Rel 'False) 'Path.Dir rep)
    (Path ('Path.Rel 'True) rep)
    (Path ('Path.Rel 'True) rep)
  where
  parent </> child = mapPath (parent </>) child

instance
  Path.PartialOps
    (Path.Path 'Path.Abs 'Path.Dir rep)
    (Path ('Path.Rel 'True) rep)
    (Path 'Path.Abs rep)
  where
  parent </?> child = traversePath (parent </?>) child

path ::
  (Path.Path rel 'Path.Dir rep -> a) ->
  (Path.Path rel 'Path.File rep -> a) ->
  Path rel rep ->
  a
path dirFn fileFn = unambiguous (dirFn . unflip) (fileFn . unflip) . getCompose

partitionPaths ::
  ( Foldable g,
    Functor g,
    Alternative h,
    Semigroup (h (Flip (Path.Path rel) rep 'Path.Dir)),
    Semigroup (h (Flip (Path.Path rel) rep 'Path.File))
  ) =>
  g (Path rel rep) ->
  (h (Path.Path rel 'Path.Dir rep), h (Path.Path rel 'Path.File rep))
partitionPaths =
  bimap (unflip <$>) (unflip <$>) . partition . fmap getCompose

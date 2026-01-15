{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2025 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- This provides a path type that will hold any of the myriad `Path.Path` types
-- that the library provides. These are created by `Data.Path.Parser.path`.
module Data.Path.Any
  ( Path,
    mapPath,
    path,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Functor.Compose (Compose (Compose), getCompose)
import "base" Data.Kind qualified as Kind
import "pathway-internal" Data.Path.Internal ((</>), (</?>))
import "pathway-internal" Data.Path.Internal qualified as Path
import "this" Data.Path.Anchored (Anchored, anchored)
import "this" Data.Path.Anchored qualified as Anchored
import "this" Data.Path.Functor
  ( Flip (Flip),
    Flip1 (Flip1),
    dmap,
    dtraverse,
    unflip,
    unflip1,
  )
import "this" Data.Path.NonReparented qualified as NonReparented
import "this" Data.Path.Unambiguous (Unambiguous, unambiguous)
import "this" Data.Path.Unambiguous qualified as Unambiguous

-- | This is a path where we don’t know (at the type level) whether it’s
--   anchored, or whether it’s a file.
--
--   There are not many operations on this type, but it is the type used for
--   generic operations like parsing and printing, with separate functions (like
--  `anchor`) to lift the contained information to the type level.
--
--   It can be pattern-matched directly if you have code paths for various path
--   types, but there are also `Data.Path.lift`, ``Data.Path.liftRelativity`,
--   and `Data.Path.liftType` operations to help with the more common cases.
--
--   A path where neither the `Relativity` nor `Type` is tracked in the type
--   (but can be checked at runtime).
--
--   This is the type that runtime-read items arrive as.
type Path :: Kind.Type -> Kind.Type
type Path rep =
  Anchored (Compose (Compose Unambiguous (Flip1 Flip rep)) Path.Path)

mapPath ::
  (forall rel typ. Path.Path rel typ rep -> Path.Path rel typ rep') ->
  Path rep ->
  Path rep'
mapPath f =
  dmap $
    Compose
      . Compose
      . dmap (Flip1 . Flip . f . unflip . unflip1)
      . getCompose
      . getCompose

instance
  Path.TotalOps
    (Anchored.Path 'Path.Dir rep)
    (Unambiguous.Path ('Path.Rel 'False) rep)
    (Path rep)
  where
  parent </> child =
    dmap
      ( \(Flip (Flip1 par)) ->
          Compose . Compose . dmap (Flip1 . Flip . (par </>) . unflip) $
            getCompose child
      )
      $ getCompose parent

instance
  Path.TotalOps
    (NonReparented.Path 'Path.Dir rep)
    (Unambiguous.Path ('Path.Rel 'False) rep)
    (Path rep)
  where
  parent </> child =
    NonReparented.weaken
      . dmap
        ( \(Flip (Flip1 par)) ->
            Compose . Compose . dmap (Flip1 . Flip . (par </>) . unflip) $
              getCompose child
        )
      $ getCompose parent

-- | We don’t know if the parent path is absolute, so we have to assume
--   concatenating a reparented path onto it could fail.
instance
  Path.PartialOps
    (Anchored.Path 'Path.Dir rep)
    (Unambiguous.Path ('Path.Rel 'True) rep)
    (Path rep)
  where
  parent </?> child =
    -- TODO: I think there must be a less verbose implementation of this that
    --       unifies the cases under the @`Path.PartialOps` (`Anchored.Path`
    --       '`Path.Dir` rep) (`Path.Path` ('`Path.Rel` '`True`) typ rep)
    --       (`Anchored.Path` typ rep)@ instance.
    Anchored.path
      ( \par ->
          fmap (Anchored.Absolute . Compose . Compose)
            . dtraverse (fmap (Flip1 . Flip) . (par </?>) . unflip)
            $ getCompose child
      )
      ( \par ->
          pure
            . Anchored.Reparented
            . Compose
            . Compose
            . dmap (Flip1 . Flip . (par </>) . unflip)
            $ getCompose child
      )
      ( \par ->
          pure
            . Anchored.Reparented
            . Compose
            . Compose
            . dmap (Flip1 . Flip . (par </>) . unflip)
            $ getCompose child
      )
      parent

-- | We don’t know if the parent path is absolute, so we have to assume
--   concatenating a reparented path onto it could fail.
instance
  Path.PartialOps
    (NonReparented.Path 'Path.Dir rep)
    (Unambiguous.Path ('Path.Rel 'True) rep)
    (Path rep)
  where
  (</?>) = (</?>) . NonReparented.weakenPath

path ::
  (Path.Path 'Path.Abs 'Path.Dir rep -> a) ->
  (Path.Path 'Path.Abs 'Path.File rep -> a) ->
  (Path.Path ('Path.Rel 'False) 'Path.Dir rep -> a) ->
  (Path.Path ('Path.Rel 'False) 'Path.File rep -> a) ->
  (Path.Path ('Path.Rel 'True) 'Path.Dir rep -> a) ->
  (Path.Path ('Path.Rel 'True) 'Path.File rep -> a) ->
  Path rep ->
  a
path absDirFn absFileFn relDirFn relFileFn repDirFn repFileFn =
  anchored
    (unamb absDirFn absFileFn)
    (unamb relDirFn relFileFn)
    (unamb repDirFn repFileFn)
  where
    unamb dirFn fileFn =
      unambiguous (dirFn . unflip . unflip1) (fileFn . unflip . unflip1)
        . getCompose
        . getCompose

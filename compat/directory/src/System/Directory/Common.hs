{-# LANGUAGE Trustworthy #-}

-- | This provides an API similar to "System.Directory", but for Pathway types.
--
--   Some differences:
-- - operations mostly require absolute paths, the “current directory” is not
--   implicit (operations may _return_ relative paths, but they will be relative
--   to an argument). The reason for returning relative paths, is because it’s a
--   total operation (once we separate reparented paths at the type level) to
--   concat the relative path to the path passed in (creating the absolute path
--   that would be returned), but a partial operation to convert a returned
--   absolute path to the same relative path.
-- - there is no `Dir.makeAbsolute`, to do the same thing,
--   @(`</?>` myPath) `<$>` `getCurrentDirectory`@ or similar will work. This
--   just makes all paths explicit, even if they do end up relative to the
--  “current” path. __TODO__: Might be worth removing the idea of a “current”
--   directory altogether?
-- - similarly, no `makeRelativeToCurrentDirectory`
-- - this includes some exception handlers for dealing with filesystem-specific
--   meanings of different `IOError`s.
--
--   One reason for enforcing the absoluteness of paths, is that paths are often
--   reported to users, and often without enough context. This tries to ensure
--   that there is at least a full path available (unless the developer makes an
--   effort to remove it.
module System.Directory.Common
  ( Operations (..),
    Permissions,
  )
where

import safe "base" Data.Bool (Bool (True))
import safe "base" Data.Either (Either)
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Ord (Ord)
import safe "base" System.IO (IO)
import "directory" System.Directory (Permissions)
import safe "pathway" Data.Path (Path, Relativity (Abs, Rel), Type, Typey)
import safe "pathway" Data.Path.Relativity qualified as Rel
import safe "pathway-compat-filepath" Common (InternalFailure)

type Operations :: Kind.Type -> Type -> Kind.Constraint
class (Typey typ) => Operations rep typ where
  canonicalizePath ::
    (Ord e) =>
    Path 'Abs typ rep ->
    IO (Either (InternalFailure rep e) (Path 'Abs typ rep))

  -- | Don’t use this. It relies on the “current directory”.
  makeAbsolute ::
    (Ord e) =>
    Path ('Rel 'True) typ rep ->
    IO (Either (InternalFailure rep e) (Path 'Abs typ rep))

  makeRelativeToCurrentDirectory ::
    (Ord e) =>
    Path 'Abs typ rep ->
    IO (Either (InternalFailure rep e) (Path ('Rel 'True) typ rep))

  getSymbolicLinkTarget ::
    (Ord e) =>
    Path 'Abs typ rep ->
    IO (Either (InternalFailure rep e) (Path 'Rel.Any typ rep))

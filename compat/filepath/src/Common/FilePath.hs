{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested @`Show` (`MP.Token` rep)@ constraints.
{-# LANGUAGE UndecidableInstances #-}

-- | This provides an API similar to "System.FilePath", but for Pathway types.
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
module Common.FilePath
  ( toPathRep,
    fromPathRep,
    handleAnchoredPath,
    absDirFromPathRep,
    anyDirFromPathRep,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Either (Either (Left), either)
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Ord (Ord)
import "base" Data.String (String)
import "base" System.IO (FilePath)
import "megaparsec" Text.Megaparsec qualified as MP
import "pathway" Data.Path
  ( Anchored (AbsDir, AbsFile, RelDir, RelFile, ReparentedDir, ReparentedFile),
    Path,
    Pathy,
    Relativity (Abs, Rel),
    Type (Dir, File),
    anchor,
    forgetRelativity,
    toText,
    unanchor,
  )
import "pathway" Data.Path.Format qualified as Format
import "pathway" Data.Path.Parser qualified as Parser
import "pathway" Data.Path.Relativity qualified as Rel (Relativity (Any))
import "this" Common (InternalFailure (IncorrectResultType, ParseFailure))

toPathRep :: (Pathy rel typ) => Path rel typ String -> FilePath
toPathRep = toText Format.local

fromPathRep ::
  (Ord e) =>
  FilePath ->
  Either (MP.ParseErrorBundle FilePath e) (Anchored String)
fromPathRep = fmap anchor . MP.parse (Parser.path Format.local) ""

handleAnchoredPath ::
  (Ord e) =>
  (Anchored String -> Either (InternalFailure FilePath e) a) ->
  FilePath ->
  Either (InternalFailure FilePath e) a
handleAnchoredPath handler = either (Left . ParseFailure) handler . fromPathRep

absDirFromPathRep ::
  (Ord e) =>
  FilePath ->
  Either (InternalFailure FilePath e) (Path 'Abs 'Dir String)
absDirFromPathRep =
  let badType rel typ = Left . IncorrectResultType Abs Dir rel typ
   in handleAnchoredPath \case
        AbsDir path -> pure path
        AbsFile path -> badType Abs File $ unanchor path
        RelDir path -> badType (Rel False) Dir $ unanchor path
        RelFile path -> badType (Rel False) File $ unanchor path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

anyDirFromPathRep ::
  (Ord e) =>
  FilePath ->
  Either (InternalFailure FilePath e) (Path 'Rel.Any 'Dir String)
anyDirFromPathRep =
  let badType rel typ = Left . IncorrectResultType Rel.Any Dir rel typ
   in handleAnchoredPath \case
        AbsDir path -> pure $ forgetRelativity path
        AbsFile path -> badType Abs File $ unanchor path
        RelDir path -> pure $ forgetRelativity path
        RelFile path -> badType (Rel False) File $ unanchor path
        ReparentedDir path -> pure $ forgetRelativity path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

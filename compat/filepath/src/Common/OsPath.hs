{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
-- __NB__: Because of the nested @`Show` (`MP.Token` rep)@ constraints.
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This provides an API similar to "System.OsPath", but for Pathway types.
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
module Common.OsPath
  ( localFormat,
    toPathRep,
    fromPathRepDir,
    handleAnchoredDir,
    absDirFromPathRep,
    anyDirFromPathRep,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bifunctor (bimap)
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Foldable (length, null)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Monoid (mempty)
import safe "base" Data.Ord (Ord)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "extra" Data.List.Extra qualified as List
import "filepath" System.OsPath qualified as O
import "filepath" System.OsPath.Types (OsChar, OsPath, OsString)
import safe "megaparsec" Text.Megaparsec qualified as MP
import safe "pathway" Data.Path
  ( Anchored (AbsDir, AbsFile, RelDir, RelFile, ReparentedDir, ReparentedFile),
    Path,
    Pathy,
    Relativity (Abs, Rel),
    Type (Dir, File),
    anchor,
    forgetRelativity,
    forgetType,
    toText,
    unanchor,
  )
import safe "pathway" Data.Path qualified as Path
import safe "pathway" Data.Path.Format (Format (Format))
import safe "pathway" Data.Path.Format qualified as Format
import safe "pathway" Data.Path.Parser qualified as Parser
import safe "pathway" Data.Path.Relativity qualified as Rel (Relativity (Any))
import safe "pathway-compat-base" Common
  ( InternalFailure (IncorrectResultType, ParseFailure),
  )

instance Path.Substible OsString where
  replace i o = O.pack . List.replace (O.unpack i) (O.unpack o) . O.unpack

-- | This is different from the standard POSIX format, because it outputs
--   without `./` for relative directories.
localFormat :: Format OsString
localFormat =
  Format
    { Format.root = O.pack . pure $ O.pathSeparator,
      Format.current = mempty,
      -- TODO: Make this safe, or eliminate for resolved paths somehow.
      Format.parent = O.unsafeEncodeUtf "..",
      Format.separator = O.pack . pure $ O.pathSeparator,
      Format.substitutions = mempty
    }

toPathRep :: (Pathy rel typ) => Path rel typ OsString -> OsPath
toPathRep = toText localFormat

instance MP.Stream OsString where
  type Token OsString = OsChar
  type Tokens OsString = OsString
  tokenToChunk Proxy = O.pack . pure
  tokensToChunk Proxy = O.pack
  chunkToTokens Proxy = O.unpack
  chunkLength Proxy = length . O.unpack
  chunkEmpty Proxy = null . O.unpack
  take1_ = fmap (O.pack <$>) . MP.take1_ . O.unpack
  takeN_ n = fmap (bimap O.pack O.pack) . MP.takeN_ n . O.unpack
  takeWhile_ p = bimap O.pack O.pack . MP.takeWhile_ p . O.unpack

fromPathRepDir ::
  (Ord e) =>
  OsPath ->
  Either (MP.ParseErrorBundle OsPath e) (Anchored OsString)
fromPathRepDir =
  fmap (anchor . forgetType) . MP.parse (Parser.directory localFormat) ""

handleAnchoredDir ::
  (Ord e) =>
  (Anchored OsString -> Either (InternalFailure OsPath e) a) ->
  OsPath ->
  Either (InternalFailure OsPath e) a
handleAnchoredDir handler = either (Left . ParseFailure) handler . fromPathRepDir

absDirFromPathRep ::
  (Ord e) =>
  OsPath ->
  Either (InternalFailure OsPath e) (Path 'Abs 'Dir OsString)
absDirFromPathRep =
  let badType rel typ = Left . IncorrectResultType Abs Dir rel typ
   in handleAnchoredDir \case
        AbsDir path -> pure path
        AbsFile path -> badType Abs File $ unanchor path
        RelDir path -> badType (Rel False) Dir $ unanchor path
        RelFile path -> badType (Rel False) File $ unanchor path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

anyDirFromPathRep ::
  (Ord e) =>
  OsPath ->
  Either (InternalFailure OsPath e) (Path 'Rel.Any 'Dir OsString)
anyDirFromPathRep =
  let badType rel typ = Left . IncorrectResultType Rel.Any Dir rel typ
   in handleAnchoredDir \case
        AbsDir path -> pure $ forgetRelativity path
        AbsFile path -> badType Abs File $ unanchor path
        RelDir path -> pure $ forgetRelativity path
        RelFile path -> badType (Rel False) File $ unanchor path
        ReparentedDir path -> pure $ forgetRelativity path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

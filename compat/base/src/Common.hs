{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested @`Show` (`MP.Token` rep)@ constraints.
{-# LANGUAGE UndecidableInstances #-}

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
module Common
  ( InternalFailure (..),
    toPathRep,
    fromPathRep,
    handleAnchoredPath,
    absFileFromPathRep,
    fileFromPathRep,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Exception (Exception)
import "base" Data.Bool (Bool (False, True))
import "base" Data.Either (Either (Left), either)
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Kind qualified as Kind
import "base" Data.Ord (Ord)
import "base" Data.String (String)
import "base" Data.Typeable (Typeable)
import "base" GHC.Generics (Generic)
import "base" System.IO (FilePath)
import "base" Text.Show (Show)
import "megaparsec" Text.Megaparsec qualified as MP
import "pathway" Data.Path
  ( Anchored (AbsDir, AbsFile, RelDir, RelFile, ReparentedDir, ReparentedFile),
    AnyPath,
    Path,
    Pathy,
    Relativity (Abs, Rel),
    Type (Dir, File),
    anchor,
    toText,
    unanchor,
  )
import "pathway" Data.Path.Format qualified as Format
import "pathway" Data.Path.Parser qualified as Parser

type InternalFailure :: Kind.Type -> Kind.Type -> Kind.Type
data InternalFailure rep e
  = ParseFailure (MP.ParseErrorBundle rep e)
  | IncorrectResultType Relativity Type Relativity Type (AnyPath rep)
  deriving stock (Generic)

deriving stock instance
  (Eq rep, Eq (MP.Token rep), Eq e) => Eq (InternalFailure rep e)

deriving stock instance
  (Show rep, Show (MP.Token rep), Show e) => Show (InternalFailure rep e)

instance
  (Show rep, Typeable rep, Show (MP.Token rep), Show e, Typeable e) =>
  Exception (InternalFailure rep e)

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

absFileFromPathRep ::
  (Ord e) =>
  FilePath ->
  Either (InternalFailure FilePath e) (Path 'Abs 'File String)
absFileFromPathRep =
  let badType rel typ = Left . IncorrectResultType Abs File rel typ
   in handleAnchoredPath \case
        AbsDir path -> badType Abs Dir $ unanchor path
        AbsFile path -> pure path
        RelDir path -> badType (Rel False) Dir $ unanchor path
        RelFile path -> badType (Rel False) File $ unanchor path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

fileFromPathRep ::
  (Ord e) =>
  FilePath ->
  Either
    (InternalFailure FilePath e)
    (Either (Path 'Abs 'File String) (Path ('Rel 'False) 'File String))
fileFromPathRep =
  let badType rel typ = Left . IncorrectResultType Abs File rel typ
   in handleAnchoredPath \case
        AbsDir path -> badType Abs Dir $ unanchor path
        AbsFile path -> pure $ Left path
        RelDir path -> badType (Rel False) Dir $ unanchor path
        RelFile path -> pure $ pure path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

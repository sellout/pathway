{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
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
module Common
  ( Rep (..),
    InternalFailure (..),
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category (id)
import safe "base" Control.Exception (Exception)
import safe "base" Data.Char qualified as Base
import safe "base" Data.Eq (Eq)
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Monoid (Monoid)
import safe "base" Data.String (String)
import safe "base" Data.Typeable (Typeable)
import safe "base" GHC.Generics (Generic)
import safe "base" System.IO (IO)
import safe "base" Text.Show (Show)
import safe "filepath" System.FilePath qualified as F.Path
import "filepath" System.OsPath qualified as O.Path
import "filepath" System.OsPath.Types (OsChar, OsString)
import safe "megaparsec" Text.Megaparsec qualified as MP
import safe "pathway" Data.Path (AnyPath, Relativity, Type)

type Rep :: Kind.Type -> Kind.Constraint
class (Monoid a) => Rep (a :: Kind.Type) where
  type Char a
  fromStringLiteral :: String -> IO a
  joinPath :: [a] -> a
  pack :: [Char a] -> a
  pathSeparator :: proxy a -> Char a
  (</>) :: a -> a -> a

instance Rep OsString where
  type Char OsString = OsChar
  fromStringLiteral = O.Path.encodeUtf
  joinPath = O.Path.joinPath
  pack = O.Path.pack
  pathSeparator _ = O.Path.pathSeparator
  (</>) = (O.Path.</>)

instance Rep String where
  type Char String = Base.Char
  fromStringLiteral = pure
  joinPath = F.Path.joinPath
  pack = id
  pathSeparator _ = F.Path.pathSeparator
  (</>) = (F.Path.</>)

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

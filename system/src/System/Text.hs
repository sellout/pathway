{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

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
module System.Text
  ( Rep (..),
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category (id)
import safe "base" Data.Char qualified as Base
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Maybe (Maybe)
import safe "base" Data.Monoid (Monoid)
import safe "base" Data.Ord (Ord)
import safe "base" Data.String (String)
import safe "base" GHC.Generics (Generic)
import safe "base" System.IO (IO)
import safe "base" Text.Show (Show)
import "directory" System.Directory.Internal qualified as O.In
import safe "pathway-compat-base" System.Environment.Pathway qualified as F.Env
import safe "pathway-compat-filepath" System.FilePath.Pathway qualified as F.Path
import "pathway-compat-filepath" System.OsPath.Pathway (OsChar, OsString)
import "pathway-compat-filepath" System.OsPath.Pathway qualified as O.Path

type Rep :: Kind.Type -> Kind.Constraint
class (Ord a, Generic a, Monoid a, Show a) => Rep (a :: Kind.Type) where
  type Char a
  encodeString :: String -> IO a
  lookupEnv :: a -> IO (Maybe a)
  pack :: [Char a] -> a
  pathSeparator :: proxy a -> Char a

instance Rep String where
  type Char String = Base.Char
  encodeString = pure
  lookupEnv = F.Env.lookupEnv
  pack = id
  pathSeparator _ = F.Path.pathSeparator

instance Rep OsString where
  type Char OsString = OsChar
  encodeString = O.Path.encodeUtf
  lookupEnv = O.In.lookupEnvOs
  pack = O.Path.pack
  pathSeparator _ = O.Path.pathSeparator

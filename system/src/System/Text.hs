{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
-- This module is inferred @Safe@ in some dependency solutions.
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
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
import safe "pathway-compat-base" System.Environment.Pathway qualified as F.Env
import safe "pathway-compat-filepath" System.FilePath.Pathway qualified as F.Path
#if MIN_VERSION_GLASGOW_HASKELL(9, 6, 1, 0)
import "directory" System.Directory.Internal qualified as O.In
import "pathway-compat-filepath" System.OsPath.Pathway (OsChar, OsString)
import "pathway-compat-filepath" System.OsPath.Pathway qualified as O.Path
#endif

-- | The representations of text that the system’s paths can be built from.
--
-- @since 99999
type Rep :: Kind.Type -> Kind.Constraint
class (Ord a, Generic a, Monoid a, Show a) => Rep (a :: Kind.Type) where
  -- | The type of the characters of @a@.
  --
  -- @since 99999
  type Char a

  -- | Encode a `String` in this representation.
  --
  -- @since 99999
  encodeString :: String -> IO a

  -- | `System.Environment.lookupEnv`, in this representation.
  --
  -- @since 99999
  lookupEnv :: a -> IO (Maybe a)

  -- | Build a value from its characters.
  --
  -- @since 99999
  pack :: [Char a] -> a

  -- | The character that separates path components on this system.
  --
  -- @since 99999
  pathSeparator :: proxy a -> Char a

-- | @since 99999
instance Rep String where
  type Char String = Base.Char
  encodeString = pure
  lookupEnv = F.Env.lookupEnv
  pack = id
  pathSeparator _ = F.Path.pathSeparator

#if MIN_VERSION_GLASGOW_HASKELL(9, 6, 1, 0)
-- | @since 99999
instance Rep OsString where
  type Char OsString = OsChar
  encodeString = O.Path.encodeUtf
  lookupEnv = O.In.lookupEnvOs
  pack = O.Path.pack
  pathSeparator _ = O.Path.pathSeparator
#endif

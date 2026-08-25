{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
-- This module is inferred @Safe@ in some dependency solutions.
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

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

#if MIN_VERSION_GLASGOW_HASKELL(9, 6, 1, 0)
instance Rep OsString where
  type Char OsString = OsChar
  encodeString = O.Path.encodeUtf
  lookupEnv = O.In.lookupEnvOs
  pack = O.Path.pack
  pathSeparator _ = O.Path.pathSeparator
#endif

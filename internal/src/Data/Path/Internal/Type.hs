{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

module Data.Path.Internal.Type (Type (..)) where

import safe "base" Data.Eq (Eq)
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Ord (Ord)
import safe "base" GHC.Generics (Generic)
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show)

-- | The `Type` type encodes the type of entity a given `Path` refers to, e.g. a
--  `File` or a `Dir`ectory; if this information is not available at compile time,
--  that's encoded by `Any`.
type Type :: Kind.Type
data Type
  = -- | A file
    File
  | -- | A directory
    Dir
  | -- | `Type` unknown until runtime
    Any
  deriving stock (Eq, Generic, Ord, Read, Show)

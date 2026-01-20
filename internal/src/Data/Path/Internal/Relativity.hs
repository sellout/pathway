{-# LANGUAGE Safe #-}

module Data.Path.Internal.Relativity (Relativity (..)) where

import "base" Data.Bool (Bool ())
import "base" Data.Eq (Eq)
import "base" Data.Kind qualified as Kind
import "base" Data.Ord (Ord)
import "base" GHC.Generics (Generic)
import "base" Text.Read (Read)
import "base" Text.Show (Show)

-- | The `Relativity` type encodes whether a given `Path` is known at compile time
--  to be `Abs`olute or `Rel`ative; if not, the constructor `Any` encodes the fact
--  that such information will only be available at runtime.
--
--  The `Rel` constructor accepts a @Bool@ which encodes whether or not the
--  `Path` has a leading @../@. TODO clarify what's True and what's False.
--
--   __TODO__: Instead of `Bool` this could perhaps be a `Nat`, which would
--             indicate the maximum number of leading @../@ in the path (I think
--             things like `minimalRoute*` prevent us from tracking the /exact/
--             number – and parsing arbitrary paths might make even the relative
--             case untenable – perhaps @`Rel` (`Maybe` `Nat`)@, where `Nothing`
--             indicates an unknown number of reparentings). That could later
--             allow for more total operations, if we add some way to track the
--             number of components in an absolute path.
type Relativity :: Kind.Type
data Relativity
  = -- | Absolute path
    Abs
  | -- | Relative path
    Rel Bool
  | -- | `Relativity` unknown until runtime
    Any
  deriving stock (Eq, Generic, Ord, Read, Show)

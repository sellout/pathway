{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
-- WAIT: "NoRecursion" doesn’t currently provide a way to ignore names like
--       `$dPopVariant_aei8`, but it should.
{-# OPTIONS_GHC -fplugin-opt NoRecursion:allow-recursion:true #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module System.IO.Caught
  ( OpenFileFailure,
    openFile,

    -- * internal
    recoverOpenFileFailure,
  )
where

import safe "base" Control.Applicative (empty, pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bool (Bool (True))
import safe "base" Data.Either (Either)
import safe "base" Data.Function (($))
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Maybe (Maybe)
import safe "base" Data.String (String)
import safe "base" System.IO (Handle, IO, IOMode)
import safe "exceptions" Control.Monad.Catch (tryJust)
import safe "pathway" Data.Path (Path, Relativity (Abs), Type (File))
import "variant" Data.Variant (V, toVariant)
import safe "this" System.IO.Error.Pathway
  ( AlreadyInUseError (AlreadyInUseError),
    DoesNotExistError (DoesNotExistError),
    IOError,
    PermissionError (PermissionError),
  )
import safe "this" System.IO.Error.Pathway qualified as IO
import safe "this" System.IO.Thin qualified as Thin

-- | The errors that opening a file can raise.
--
-- @since 99999
type OpenFileFailure :: [Kind.Type]
type OpenFileFailure = '[AlreadyInUseError, DoesNotExistError, PermissionError]

-- | Classify an `IOError` as one of the `OpenFileFailure`s, or `Nothing` if it
--   isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverOpenFileFailure :: IOError -> Maybe (V OpenFileFailure)
recoverOpenFileFailure ioe =
  if
    | IO.isAlreadyInUseError ioe -> pure . toVariant $ AlreadyInUseError ioe
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | True -> empty

-- | `System.IO.openFile`, on `Path`s, with the `IOException`s it’s known to
--   raise returned as variants instead.
--
-- @since 99999
openFile ::
  Path 'Abs 'File String -> IOMode -> IO (Either (V OpenFileFailure) Handle)
openFile filePath = tryJust recoverOpenFileFailure . Thin.openFile filePath

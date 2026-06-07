{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
-- TODO: Complains about the following names, which have no way to be silenced
--       yet: $dPopVariant_ad2Z, $dPopVariant_ad3L, $dPopVariant_ad4s
{-# OPTIONS_GHC -fplugin-opt NoRecursion:allow-recursion:true #-}

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
import safe "this" System.IO.Error.Caught
  ( AlreadyInUseError (AlreadyInUseError),
    DoesNotExistError (DoesNotExistError),
    PermissionError (PermissionError),
  )
import safe "this" System.IO.Error.Pathway (IOError)
import safe "this" System.IO.Error.Pathway qualified as IO
import safe "this" System.IO.Thin qualified as Thin

type OpenFileFailure :: [Kind.Type]
type OpenFileFailure = '[AlreadyInUseError, DoesNotExistError, PermissionError]

recoverOpenFileFailure :: IOError -> Maybe (V OpenFileFailure)
recoverOpenFileFailure ioe =
  if
    | IO.isAlreadyInUseError ioe -> pure . toVariant $ AlreadyInUseError ioe
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | True -> empty

openFile ::
  Path 'Abs 'File String -> IOMode -> IO (Either (V OpenFileFailure) Handle)
openFile filePath = tryJust recoverOpenFileFailure . Thin.openFile filePath

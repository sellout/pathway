{-# LANGUAGE Trustworthy #-}

module System.File.OsPath.Caught
  ( OpenFileFailure,
    openFile,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Data.Either (Either)
import safe "base" System.IO (Handle, IO, IOMode)
import safe "exceptions" Control.Monad.Catch (tryJust)
import safe "filepath" System.OsPath.Types (OsString)
import safe "pathway" Data.Path (Path, Relativity (Abs), Type (File))
import "pathway-compat-base" System.IO.Caught
  ( OpenFileFailure,
    recoverOpenFileFailure,
  )
import "variant" Data.Variant (V)
import safe "this" System.File.OsPath.Thin qualified as Thin

openFile ::
  Path 'Abs 'File OsString -> IOMode -> IO (Either (V OpenFileFailure) Handle)
openFile filePath = tryJust recoverOpenFileFailure . Thin.openFile filePath

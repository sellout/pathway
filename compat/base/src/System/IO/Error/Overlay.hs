{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module System.IO.Error.Overlay
  ( mkIOError,
    annotateIOError,
    ioeGetFileName,
    ioeSetFileName,
  )
where

import "base" Control.Applicative (empty, pure)
import "base" Control.Monad ((<=<))
import "base" Data.Either (either)
import "base" Data.Function (const)
import "base" Data.Maybe (Maybe)
import "base" Data.String (String)
import "base" Data.Void (Void)
import "base" System.IO.Error (IOError)
import "pathway" Data.Path (Path, Relativity (Abs), Type (File))
import "this" System.IO.Error.Thin (annotateIOError, ioeSetFileName, mkIOError)
import "this" System.IO.Error.Thin qualified as Thin

ioeGetFileName :: IOError -> Maybe (Path 'Abs 'File String)
ioeGetFileName = either (const empty) pure <=< Thin.ioeGetFileName @Void

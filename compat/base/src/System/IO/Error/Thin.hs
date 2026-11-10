{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module System.IO.Error.Thin
  ( mkIOError,
    annotateIOError,
    ioeGetFileName,
    ioeSetFileName,
  )
where

import "base" Control.Category ((.))
import "base" Data.Either (Either)
import "base" Data.Functor (fmap)
import "base" Data.Maybe (Maybe)
import "base" Data.Ord (Ord)
import "base" Data.String (String)
import "base" System.IO (FilePath, Handle)
import "base" System.IO.Error (IOError, IOErrorType)
import "base" System.IO.Error qualified as IO
import "pathway" Data.Path (Path, Relativity (Abs), Type (File))
import "this" Common (InternalFailure, absFileFromPathRep, toPathRep)

-- | `System.IO.Error.mkIOError`, on `Path`s.
--
-- @since 99999
mkIOError ::
  IOErrorType ->
  String ->
  Maybe Handle ->
  Maybe (Path 'Abs 'File String) ->
  IOError
mkIOError ty str h = IO.mkIOError ty str h . fmap toPathRep

-- | `System.IO.Error.annotateIOError`, on `Path`s.
--
-- @since 99999
annotateIOError ::
  IOError -> String -> Maybe Handle -> Maybe (Path 'Abs 'File String) -> IOError
annotateIOError ioe str h = IO.annotateIOError ioe str h . fmap toPathRep

-- | `System.IO.Error.ioeGetFileName`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
ioeGetFileName ::
  (Ord e) =>
  IOError ->
  Maybe (Either (InternalFailure FilePath e) (Path 'Abs 'File String))
ioeGetFileName = fmap absFileFromPathRep . IO.ioeGetFileName

-- | `System.IO.Error.ioeSetFileName`, on `Path`s.
--
-- @since 99999
ioeSetFileName :: IOError -> Path 'Abs 'File String -> IOError
ioeSetFileName ioe = IO.ioeSetFileName ioe . toPathRep

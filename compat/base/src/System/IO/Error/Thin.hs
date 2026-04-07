{-# LANGUAGE Safe #-}

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

mkIOError ::
  IOErrorType ->
  String ->
  Maybe Handle ->
  Maybe (Path 'Abs 'File String) ->
  IOError
mkIOError ty str h = IO.mkIOError ty str h . fmap toPathRep

annotateIOError ::
  IOError -> String -> Maybe Handle -> Maybe (Path 'Abs 'File String) -> IOError
annotateIOError ioe str h = IO.annotateIOError ioe str h . fmap toPathRep

ioeGetFileName ::
  (Ord e) =>
  IOError ->
  Maybe (Either (InternalFailure FilePath e) (Path 'Abs 'File String))
ioeGetFileName = fmap absFileFromPathRep . IO.ioeGetFileName

ioeSetFileName :: IOError -> Path 'Abs 'File String -> IOError
ioeSetFileName ioe = IO.ioeSetFileName ioe . toPathRep

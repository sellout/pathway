{-# LANGUAGE Safe #-}

module Data.Path.File
  ( basename,
    directory,
  )
where

import "base" Control.Category (Category ((.)))
import "base" Data.Functor.Const (Const (Const))
import "base" Data.Functor.Identity (Identity (runIdentity))
import "pathway-internal" Data.Path.Internal
  ( Path (Path, directories, filename, parents),
    Type (Dir, File),
  )

basename :: Path rel 'File rep -> rep
basename = runIdentity . filename

directory :: Path rel 'File rep -> Path rel 'Dir rep
directory file =
  Path {parents = parents file, directories = directories file, filename = Const ()}

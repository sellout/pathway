{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Path.File
  ( basename,
    directory,
  )
where

import "base" Control.Category ((.))
import "base" Data.Functor.Const (Const (Const))
import "base" Data.Functor.Identity (runIdentity)
import "pathway-internal" Data.Path.Internal
  ( Path (Path),
    Type (Dir, File),
    directories,
    filename,
    parents,
  )

basename :: Path rel 'File rep -> rep
basename = runIdentity . filename

directory :: Path rel 'File rep -> Path rel 'Dir rep
directory file =
  Path
    { parents = parents file,
      directories = directories file,
      filename = Const ()
    }

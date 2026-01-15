{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Path.Directory
  ( ascendAbsolute,
    (</>),
    (</?>),
    current,
    isCurrent,
    root,
    isRoot,
    descendThrough,
    descendTo,
    selectFile,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Function (($))
import "base" Data.Functor.Const (Const (Const))
import "base" Data.Semigroup ((<>))
import "pathway-internal" Data.Path.Internal
  ( List,
    Path (Path),
    Relativity (Abs, Rel),
    Type (Dir, File),
    current,
    directories,
    filename,
    parents,
    (</>),
    (</?>),
  )
import "yaya" Yaya.Applied (reverse, tail)
import "yaya" Yaya.Fold (cata, embed)
import "yaya" Yaya.Pattern (Maybe (Nothing), XNor (Both, Neither), xnor)

-- | __FIXME__: Move upstream.
isNeither :: XNor a b -> Bool
isNeither = xnor True (\_ _ -> False)

-- | Move up one directory in the file hierarchy.
ascendAbsolute ::
  Path 'Abs 'Dir rep ->
  --  | `Nothing` if the provided directory @`==` `root`.
  Maybe (Path 'Abs 'Dir rep)
ascendAbsolute dir =
  if cata isNeither $ directories dir
    then Nothing
    else pure dir {directories = tail $ directories dir}

isCurrent :: Path ('Rel 'False) 'Dir rep -> Bool
isCurrent = cata isNeither . directories

descendThrough :: Path rel 'Dir rep -> List rep -> Path rel 'Dir rep
descendThrough directory newComponents =
  directory {directories = reverse newComponents <> directories directory}

descendTo :: Path rel 'Dir rep -> rep -> Path rel 'Dir rep
descendTo directory component =
  descendThrough directory . embed . Both component $ embed Neither

root :: Path 'Abs 'Dir rep
root = Path {parents = (), directories = embed Neither, filename = Const ()}

isRoot :: Path 'Abs 'Dir rep -> Bool
isRoot = cata isNeither . directories

selectFile :: Path rel 'Dir rep -> rep -> Path rel 'File rep
selectFile path fileName = path {filename = pure fileName}

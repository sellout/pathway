{-# LANGUAGE Safe #-}

module Data.Path.Directory
  ( ascendAbsolute,
    (</>),
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
    current,
    directories,
    filename,
    parents,
    (</>),
  )
import "yaya" Yaya.Applied (reverse, tail)
import "yaya" Yaya.Fold (cata, embed)
import "yaya" Yaya.Pattern (Maybe (Nothing), XNor (Both, Neither), xnor)
import "this" Data.Path.Relativity (Relativity (Abs, Rel))
import "this" Data.Path.Type (Type (Dir, File))

-- $setup
-- >>> :seti -XQuasiQuotes
-- >>> import Data.Path (toText)
-- >>> import qualified Data.Path.Format as Format
-- >>> import Data.Path.TH (posix)
-- >>> import "base" Data.Functor ((<$>))
-- >>> import "text" Data.Text (Text)

-- | __FIXME__: Move upstream.
isNeither :: XNor a b -> Bool
isNeither = xnor True (\_ _ -> False)

-- | Move up one directory in the file hierarchy.
ascendAbsolute ::
  Path res 'Abs 'Dir rep ->
  --  | `Nothing` if the provided directory @`==` `root`.
  Maybe (Path res 'Abs 'Dir rep)
ascendAbsolute dir =
  if cata isNeither $ directories dir
    then Nothing
    else pure dir {directories = tail $ directories dir}

isCurrent :: Path res 'Rel 'Dir rep -> Bool
isCurrent = cata isNeither . directories

descendThrough :: Path res rel 'Dir rep -> List rep -> Path res rel 'Dir rep
descendThrough directory newComponents =
  directory {directories = reverse newComponents <> directories directory}

descendTo :: Path res rel 'Dir rep -> rep -> Path res rel 'Dir rep
descendTo directory component =
  descendThrough directory . embed . Both component $ embed Neither

root :: Path res 'Abs 'Dir rep
root = Path {parents = (), directories = embed Neither, filename = Const ()}

isRoot :: Path res 'Abs 'Dir rep -> Bool
isRoot = cata isNeither . directories

selectFile :: Path res rel 'Dir rep -> rep -> Path res rel 'File rep
selectFile path fileName = path {filename = pure fileName}

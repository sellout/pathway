{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

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

import "base" Control.Applicative (Applicative (pure))
import "base" Control.Category (Category ((.)))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Function (($))
import "base" Data.Functor.Const (Const (Const))
import "base" Data.Ord (Ord ((<=)))
import "base" Data.Semigroup (Semigroup ((<>)))
import "pathway-internal" Data.Path.Internal
  ( List,
    Path (Path, directories, filename, parents),
    Relativity (Abs, Rel),
    Type (Dir, File),
    current,
    (</>),
  )
import "yaya" Yaya.Applied (drop, length, reverse, tail)
import "yaya" Yaya.Fold (Recursive (cata), Steppable (embed))
import "yaya" Yaya.Pattern (Maybe (Nothing), XNor (Both, Neither), xnor)

-- $setup
-- >>> :seti -XQuasiQuotes
-- >>> import "base" Data.Functor ((<$>))
-- >>> import "text" Data.Text (Text)
-- >>> import "this" Data.Path (toText)
-- >>> import qualified "this" Data.Path.Format as Format
-- >>> import "this" Data.Path.TH (posix)

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

-- | Concatenate a reparented path onto an absolute directory.
--
--  __NB__: The precedence is carefully set so that cases like the one below can
--          be written without parentheses.
--
-- >>> :{
--   toText @_ @_ @Text Format.posix
--     <$> [posix|/|] </?> [posix|user/|] </> [posix|../usr/bine/|] <> [posix|../bin/|] </> [posix|env|]
-- :}
-- Just "/usr/bin/env"
--
--          Note the four operators used: `<$>`, `</?>`, `</>`, and `<>`. Any
--          change to fixity would cause this to collapse.
(</?>) ::
  Path 'Abs 'Dir rep ->
  Path ('Rel 'True) typ rep ->
  -- | `Nothing` when the child path is reparented above the root directory.
  Maybe (Path 'Abs typ rep)
parent </?> child =
  if parents child <= length (directories parent)
    then
      pure
        Path
          { parents = (),
            directories =
              directories child <> drop (parents child) (directories parent),
            filename = filename child
          }
    else Nothing

infixr 5 </?>

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

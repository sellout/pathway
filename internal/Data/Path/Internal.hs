{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- __NB__: Because of the nested `Parents` and `Filename` constraints.
{-# LANGUAGE UndecidableInstances #-}

module Data.Path.Internal
  ( List (List),
    Parents,
    Filename,
    Path (..),
    Relativity (..),
    Type (..),
    current,
    (</>),
  )
where

import "base" Control.Category (Category ((.)))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable (foldr), sum)
import "base" Data.Function (($))
import "base" Data.Functor (Functor (fmap), (<$>))
import "base" Data.Functor.Const (Const (Const))
import "base" Data.Functor.Identity (Identity)
import "base" Data.Monoid (Monoid (mempty))
import "base" Data.Ord (Ord)
import "base" Data.Proxy (Proxy (Proxy))
import "base" Data.Semigroup (Semigroup ((<>)))
-- import "base" Data.Traversable (Traversable (traverse))
import "base" GHC.Generics (Generic, Generic1)
import "base" GHC.Natural (minusNaturalMaybe)
import "base" Numeric.Natural (Natural)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "yaya" Yaya.Applied (drop, length)
import "yaya" Yaya.Fold
  ( Mu,
    Projectable (project),
    Recursive (cata),
    Steppable (embed),
  )
import "yaya" Yaya.Functor (firstMap)
import "yaya" Yaya.Pattern (Maybe, XNor (Neither), xnor)
import "base" Prelude (Num ((+)))

-- |
--
--  __TODO__: Instead of `Bool` this could perhaps be a `Nat`, which would
--            indicate the maximum number of @../@ in the path (I think things
--            like `minimalRoute*` prevent us from tracking the /exact/ number –
--            and parsing arbitrary paths might make even the relative case
--            untenable – perhaps @`Rel` (`Maybe` `Nat`)@, where `Nothing`
--            indicates an unknown number of reparentings). That could later
--            allow for more total operations, if we add some way to track the
--            number of components in an absolute path.
data Relativity = Abs | Rel Bool | Any
  deriving stock (Eq, Generic, Ord, Read, Show)

type family Parents (rel :: Relativity) = result | result -> rel where
  Parents 'Abs = ()
  Parents ('Rel 'False) = Proxy Natural
  Parents ('Rel 'True) = Natural
  Parents 'Any = Maybe Natural

data Type = File | Dir | Pathic
  deriving stock (Eq, Generic, Ord, Read, Show)

type family Filename (typ :: Type) = result | result -> typ where
  Filename 'Dir = Const ()
  Filename 'File = Identity
  Filename 'Pathic = Maybe

-- | A strict sequence.
--
--  __TODO__: Move this upstream to Yaya.
newtype List a = List (Mu (XNor a))
  deriving stock (Eq, Generic, Ord, Read, Show)

instance Projectable (->) (List a) (XNor a) where
  project (List mu) = List <$> project mu

instance Recursive (->) (List a) (XNor a) where
  cata φ (List mu) = cata φ mu

instance Steppable (->) (List a) (XNor a) where
  embed = List . embed . fmap (\(List mu) -> mu)

instance Semigroup (List a) where
  List mu <> List mu' = List $ mu <> mu'

instance Monoid (List a) where
  mempty = List mempty

instance Foldable List where
  foldr f z (List mu) = cata (xnor z f) mu

instance Functor List where
  fmap f (List mu) = List (firstMap f mu)

-- | This provides a dozen variants of a filesystem path type, split into four
--   categories:
--
-- - six of them are the specific types that represent concrete paths;
-- - three of them represent a path where the specific `Type` is unknown at the
--   type level;
-- - two of them represent a path where the specific `Relativity` is unknown at
--   the type level; and
-- - one, given the synonym `AnyPath`, is fully general at the type level.
--
--   The @rep@ is the type used for individual path components (directory and
--   file names). This library does no manipulation of @rep@ outside of parsing
--   and printing. An @`Ord` rep@ instance is generally all that’s needed, with
--   some way to do substitution when parsing/printing. Some reasonable
--   implementions include
--
-- - "Data.Text" – if you are parsing user paths, and don’t need to store too
--   many at once
-- - "Data.Text.Short" – if you are not doing additional manipulation of the
--   components and/or you have a lot of paths to store.
-- - "System.OsPath" – if your paths represent actual filesystem paths on the
--   local system, and are not meant to be transferred to other systems.
--
--   More general information about some possible @ref@ choices can be found in
--  [The Ultimate Guide to Haskell Strings](https://hasufell.github.io/posts/2024-05-07-ultimate-string-guide.html).
--
--   There are functions to convert between the above categories (each path can
--   only be represented by a single type in each category). Note that none of
--   these paths are ambiguous in any way – it‘s only a matter of which
--   information is visible at the type level. E.g., `AnyPath` can be inspected
--   to determine if it’s absolute or relative, file or directory.
data Path rel typ rep = Path
  { parents :: Parents rel,
    directories :: List rep,
    filename :: Filename typ rep
  }
  deriving stock (Generic, Generic1)

deriving stock instance (Eq (Parents rel), Eq (Filename typ rep), Eq rep) => Eq (Path rel typ rep)

deriving stock instance (Ord (Parents rel), Ord (Filename typ rep), Ord rep) => Ord (Path rel typ rep)

deriving stock instance (Read (Parents rel), Read (Filename typ rep), Read rep) => Read (Path rel typ rep)

deriving stock instance (Show (Parents rel), Show (Filename typ rep), Show rep) => Show (Path rel typ rep)

deriving stock instance (Foldable (Filename typ)) => Foldable (Path rel typ)

deriving stock instance (Functor (Filename typ)) => Functor (Path rel typ)

-- deriving stock instance Traversable (Path rel 'Dir)

-- deriving stock instance Traversable (Path rel 'File)

class TotalOps rel par rel' | rel par -> rel' where
  -- | Concatenate two paths.
  --
  --  __NB__: The precedence is one higher than `System.FilePath.</>` and
  --         `Path.</>` so that cases like the one below can be written without
  --          parentheses.
  --
  --        > toText Format.posix
  --        >   <$> [posix|/|] </?> [posix|usr/|] </> [posix|bin/|] </> [posix|env|]
  --
  --          The precedence is tricky, because `<>` also has the same precedence,
  --          but only applies to relative paths, so you can’t mix `<>` and `</>`.
  --          E.g., ideally you’d be able to write @`toText` `<$>` abs `</?>` rel1
  --         `<>` rel2 `</>` file@ without parens, but if you could, then
  --          @`toText` `<$>` abs `</?>` rel1 `</>` rel2 `</>` file@ would need
  --          parens either @`toText` `<$>` (abs `</?>` rel1 `</>` rel2 `</>`
  --          file)@ or @`toText` `<$>` abs `</?>` (rel1 `</>` rel2 `</>` file)@.
  --          Unless you can mix an associative and non-associative operator of
  --          the same precedence …
  (</>) :: Path rel 'Dir rep -> Path ('Rel par) typ rep -> Path rel' typ rep

infixr 5 </>

instance TotalOps 'Any 'False 'Any where
  parent </> child =
    Path
      { parents = parents parent,
        directories = directories child <> directories parent,
        filename = filename child
      }

instance TotalOps ('Rel 'True) 'True ('Rel 'True) where
  parent </> child =
    Path
      { parents =
          foldr (+) (parents parent)
            . minusNaturalMaybe (parents child)
            . length
            $ directories parent,
        directories =
          directories child <> drop (parents child) (directories parent),
        filename = filename child
      }

instance TotalOps ('Rel 'True) 'False ('Rel 'True) where
  parent </> child =
    Path
      { parents = parents parent,
        directories = directories child <> directories parent,
        filename = filename child
      }

instance TotalOps ('Rel 'False) 'True ('Rel 'True) where
  parent </> child =
    Path
      { parents =
          sum . minusNaturalMaybe (parents child) . length $ directories parent,
        directories =
          directories child <> drop (parents child) (directories parent),
        filename = filename child
      }

instance TotalOps ('Rel 'False) 'False ('Rel 'False) where
  parent </> child =
    Path
      { parents = Proxy,
        directories = directories child <> directories parent,
        filename = filename child
      }

instance TotalOps 'Abs 'False 'Abs where
  parent </> child =
    Path
      { parents = (),
        directories = directories child <> directories parent,
        filename = filename child
      }

-- | This does /not/ represent the “current directory” as in the absolute path
--   from which the program was run or something, but rather the path “./”.
current :: Path ('Rel 'False) 'Dir rep
current =
  Path {parents = Proxy, directories = embed Neither, filename = Const ()}

-- | Forget that a relative path doesn’t have any parents.
weaken :: Path ('Rel 'False) typ rep -> Path ('Rel 'True) typ rep
weaken path = path {parents = 0}

-- | Only relative directories form a semigroup under `</>`.
instance Semigroup (Path ('Rel 'False) 'Dir rep) where
  (<>) = (</>)

-- | Only relative directories form a semigroup under `</>`.
instance Semigroup (Path ('Rel 'True) 'Dir rep) where
  (<>) = (</>)

-- | Only relative directories form a monoid under concatenation.
instance Monoid (Path ('Rel 'False) 'Dir rep) where
  mempty = current

-- | Only relative directories form a monoid under concatenation.
instance Monoid (Path ('Rel 'True) 'Dir rep) where
  mempty = weaken current

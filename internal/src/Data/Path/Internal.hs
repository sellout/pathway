{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- __NB__: Because of the nested `Parents` and `Filename` constraints.
{-# LANGUAGE UndecidableInstances #-}
-- "GHC.Natural" is ‘Unsafe’ before base 4.14.4. We can’t conditionalize the
-- Safe Haskell extension (because it forces Safe Haskell-using consumers to
-- conditionalize), so this silences the fact that this module is inferred
-- ‘Safe’ in some configurations. We should be able to remove this (and this
-- module made ‘Safe’) once base-4.14.4 is the oldest supported version.
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

module Data.Path.Internal
  ( List (List),
    Parents,
    Filename,
    Path (..),
    Relativity (..),
    Type (..),
    AmbiguousPath (..),
    current,
    (</>),
    -- meh
    Flip (..),
    Flip1 (..),
    Anchored (..),
    AnchoredPath,
    NonReparented (..),
    NonReparentedPath,
    Unambiguous (..),
    UnambiguousPath,
    AncUnambPath,
    DFunctor (..),
  )
where

import safe "base" Control.Category ((.))
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (Foldable, foldr, sum)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor, fmap, (<$>))
import safe "base" Data.Functor.Compose (Compose)
import safe "base" Data.Functor.Const (Const (Const))
import safe "base" Data.Functor.Identity (Identity)
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Monoid (Monoid, mempty)
import safe "base" Data.Ord (Ord)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Semigroup (Semigroup, (<>))
-- import safe "base" Data.Traversable (Traversable, traverse)
import safe "base" GHC.Generics (Generic, Generic1)
-- TODO: `minusNaturalMaybe` is exported from Numeric.Natural starting with base-4.18 (GHC 9.6).
import "base" GHC.Natural (minusNaturalMaybe)
import safe "base" Numeric.Natural (Natural)
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show)
import safe "yaya" Yaya.Applied (drop, length)
import safe "yaya" Yaya.Fold
  ( Mu,
    Projectable,
    Recursive,
    Steppable,
    cata,
    embed,
    project,
  )
import safe "yaya" Yaya.Functor (firstMap)
import safe "yaya" Yaya.Pattern (Maybe, XNor (Neither), xnor)
import safe "base" Prelude ((+))

-- | __FIXME__: I think this is in base already
type Flip :: (a -> b -> Kind.Type) -> b -> a -> Kind.Type
newtype Flip f b a = Flip {unflip :: f a b}
  deriving stock (Eq, Generic, Ord, Read, Show)

type Flip1 :: (a -> b -> c -> Kind.Type) -> b -> a -> c -> Kind.Type
newtype Flip1 f b a c = Flip1 {unflip1 :: f a b c}
  deriving stock (Eq, Generic, Ord, Read, Show)

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
type Relativity :: Kind.Type
data Relativity = Abs | Rel Bool | Any
  deriving stock (Eq, Generic, Ord, Read, Show)

type Parents :: Relativity -> Kind.Type
type family Parents rel = result | result -> rel where
  Parents 'Abs = ()
  Parents ('Rel 'False) = Proxy Natural
  Parents ('Rel 'True) = Natural
  Parents 'Any = Maybe Natural

type Type :: Kind.Type
data Type = File | Dir | Pathic
  deriving stock (Eq, Generic, Ord, Read, Show)

type Filename :: Type -> Kind.Type -> Kind.Type
type family Filename typ = result | result -> typ where
  Filename 'Dir = Const ()
  Filename 'File = Identity
  Filename 'Pathic = Maybe

-- | A strict sequence.
--
--  __TODO__: Move this upstream to Yaya.
type List :: Kind.Type -> Kind.Type
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
type Path :: Relativity -> Type -> Kind.Type -> Kind.Type
data Path rel typ rep = Path
  { parents :: Parents rel,
    directories :: List rep,
    filename :: Filename typ rep
  }
  deriving stock (Generic, Generic1)

deriving stock instance
  (Eq (Parents rel), Eq (Filename typ rep), Eq rep) => Eq (Path rel typ rep)

deriving stock instance
  (Ord (Parents rel), Ord (Filename typ rep), Ord rep) => Ord (Path rel typ rep)

deriving stock instance
  (Read (Parents rel), Read (Filename typ rep), Read rep) =>
  Read (Path rel typ rep)

deriving stock instance
  (Show (Parents rel), Show (Filename typ rep), Show rep) =>
  Show (Path rel typ rep)

deriving stock instance
  (Foldable (Filename typ)) => Foldable (Path rel typ)

deriving stock instance
  (Functor (Filename typ)) => Functor (Path rel typ)

-- deriving stock instance Traversable (Path rel 'Dir)

-- deriving stock instance Traversable (Path rel 'File)

type AmbiguousPath :: Relativity -> Kind.Type -> Kind.Type
data AmbiguousPath rel rep = AmbiguousPath
  { ambParents :: Parents rel,
    ambDirectories :: List rep,
    ambiguousComponent :: rep
  }
  deriving stock (Generic, Generic1)

-- Tradeoffs between `Path `Any 'Pathic rep` and `Anchored (UnambiguousPath rep)`:
-- - `Anchored` involves a bunch of synonyms
-- - `Anchored` keeps `Path` disjoint
-- - `Pathic` makes parameterized types match more broadly (is there any case where we want to allow `Abs` and `Rel`, but not `Any`?)
-- - `Anchored` mokes some code more complicated (parsers)
-- - need `Anchored` and `Unambiguous` anyway, for other cases

-- | Disjunction of a type parameterized over `Relativity`.
--
--  __NB__: This type may be awkward to use directly, so see `AnchoredPath` for
--          the common case.
type Anchored :: (Relativity -> Kind.Type) -> Kind.Type
data Anchored f
  = Absolute (f 'Abs)
  | Relative (f ('Rel 'False))
  | Reparented (f ('Rel 'True))
  deriving stock (Generic)

deriving stock instance (Eq (f 'Abs), Eq (f ('Rel 'False)), Eq (f ('Rel 'True))) => Eq (Anchored f)

deriving stock instance (Ord (f 'Abs), Ord (f ('Rel 'False)), Ord (f ('Rel 'True))) => Ord (Anchored f)

deriving stock instance (Read (f 'Abs), Read (f ('Rel 'False)), Read (f ('Rel 'True))) => Read (Anchored f)

deriving stock instance (Show (f 'Abs), Show (f ('Rel 'False)), Show (f ('Rel 'True))) => Show (Anchored f)

-- |
--
--  __TODO__: Generalize the type of the one in Yaya.
type DFunctor ::
  (j -> j -> Kind.Type) ->
  (k -> k -> Kind.Type) ->
  ((i -> j) -> k) ->
  Kind.Constraint
class DFunctor jCat kCat d where
  dmap :: (forall a. f a `jCat` g a) -> d f `kCat` d g

instance DFunctor (->) (->) Anchored where
  dmap f = \case
    Absolute abs -> Absolute $ f abs
    Relative rel -> Relative $ f rel
    Reparented rep -> Reparented $ f rep

-- type Path'' :: Type -> Kind.Type -> Relativity -> Kind.Type
-- type Path'' typ rep rel = Path rel typ rep

-- | A path where the `Relativity` is not tracked in the type (but can be
--   checked at runtime).
type AnchoredPath :: Type -> Kind.Type -> Kind.Type
type AnchoredPath typ rep = Anchored (Flip (Flip1 Path typ) rep)

-- -- | `AnchoredPath` with the parameters swapped.
-- type AnchoredPath' :: Kind.Type -> Type -> Kind.Type
-- type AnchoredPath' rep typ = AnchoredPath typ rep

-- |
--
--  __TODO__: Should this be a substructure of `Anchored`?
type NonReparented :: (Relativity -> Kind.Type) -> Kind.Type
data NonReparented f
  = Absolute' (f 'Abs)
  | Relative' (f ('Rel 'False))
  deriving stock (Generic)

deriving stock instance (Eq (f 'Abs), Eq (f ('Rel 'False)), Eq (f ('Rel 'True))) => Eq (NonReparented f)

deriving stock instance (Ord (f 'Abs), Ord (f ('Rel 'False)), Ord (f ('Rel 'True))) => Ord (NonReparented f)

deriving stock instance (Read (f 'Abs), Read (f ('Rel 'False)), Read (f ('Rel 'True))) => Read (NonReparented f)

deriving stock instance (Show (f 'Abs), Show (f ('Rel 'False)), Show (f ('Rel 'True))) => Show (NonReparented f)

instance DFunctor (->) (->) NonReparented where
  dmap f = \case
    Absolute' abs -> Absolute' $ f abs
    Relative' rel -> Relative' $ f rel

-- | A path where the `Relativity` is not tracked in the type (but can be
--   checked at runtime).
type NonReparentedPath :: Type -> Kind.Type -> Kind.Type
type NonReparentedPath typ rep = NonReparented (Flip (Flip1 Path typ) rep)

-- | Disjunction of a type parameterized over `Type`.
--
--  __NB__: This type may be awkward to use directly, so see `UnambiguousPath`
--          for the common case.
type Unambiguous :: (Type -> Kind.Type) -> Kind.Type
data Unambiguous f
  = Directory (f 'Dir)
  | File' (f 'File)
  deriving stock (Generic)

deriving stock instance (Eq (f 'Dir), Eq (f 'File)) => Eq (Unambiguous f)

deriving stock instance (Ord (f 'Dir), Ord (f 'File)) => Ord (Unambiguous f)

deriving stock instance (Read (f 'Dir), Read (f 'File)) => Read (Unambiguous f)

deriving stock instance (Show (f 'Dir), Show (f 'File)) => Show (Unambiguous f)

instance DFunctor (->) (->) Unambiguous where
  dmap f = \case
    Directory dir -> Directory $ f dir
    File' file -> File' $ f file

-- type Path' :: Relativity -> Kind.Type -> Type -> Kind.Type
-- type Path' rel rep typ = Path rel typ rep

-- | A path where the `Type` is not tracked in the type (but can be checked at
--   runtime).
type UnambiguousPath :: Relativity -> Kind.Type -> Kind.Type
type UnambiguousPath rel rep = Unambiguous (Flip (Path rel) rep)

-- | A path where neither the `Relativity` nor `Type` is tracked in the type
--   (but can be checked at runtime).
type AncUnambPath :: Kind.Type -> Kind.Type
type AncUnambPath rep = Anchored (Compose (Compose Unambiguous (Flip1 Flip rep)) Path)

-- type RelativePath :: Bool -> Type -> Kind.Type -> (Bool -> Relativity) -> Kind.Type
-- type RelativePath reparented typ rep f = Path (f reparented) typ rep

type TotalOps :: Relativity -> Bool -> Relativity -> Kind.Constraint
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

{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- __NB__: Because of the nested `Parents` and `Filename` constraints.
{-# LANGUAGE UndecidableInstances #-}
-- __TODO__: This is only for instances that should be moved upstream to Yaya.
{-# OPTIONS_GHC -Wno-orphans #-}
-- "GHC.Natural" is ‘Unsafe’ before base 4.14.4. We can’t conditionalize the
-- Safe Haskell extension (because it forces Safe Haskell-using consumers to
-- conditionalize), so this silences the fact that this module is inferred
-- ‘Safe’ in some configurations. We should be able to remove this (and this
-- module made ‘Safe’) once base-4.14.4 is the oldest supported version.
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- **DO NOT IMPORT THIS**
--
-- Everything here should be available in the @pathway@ package via "Data.Path".
-- Depend on that instead.
module Data.Path.Internal
  ( List (List),
    Parents,
    Filename,
    PartialOps,
    Path (..),
    Relativity (..),
    TotalOps,
    Type (..),
    current,
    (</>),
    (</?>),
  )
where

import "base" Control.Applicative (Applicative, liftA2, pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bifoldable (Bifoldable, bifoldr)
import safe "base" Data.Bitraversable (Bitraversable, bisequenceA, bitraverse)
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (Foldable, foldr, sum)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor, fmap, (<$>))
import safe "base" Data.Functor.Const (Const (Const))
import safe "base" Data.Functor.Identity (Identity)
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Monoid (Monoid, mempty)
import safe "base" Data.Ord (Ord, (<=))
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Semigroup (Semigroup, (<>))
import safe "base" Data.Traversable (Traversable, traverse)
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
import safe "yaya" Yaya.Functor (DFunctor, firstMap)
import safe "yaya" Yaya.Pattern (Maybe (Nothing), XNor (Both, Neither), xnor)
import safe "base" Prelude ((+))

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
data Relativity = Abs | Rel Bool
  deriving stock (Eq, Generic, Ord, Read, Show)

type Parents :: Relativity -> Kind.Type
type family Parents rel = result | result -> rel where
  Parents 'Abs = ()
  Parents ('Rel 'False) = Proxy Natural
  Parents ('Rel 'True) = Natural

type Type :: Kind.Type
data Type = File | Dir
  deriving stock (Eq, Generic, Ord, Read, Show)

type Filename :: Type -> Kind.Type -> Kind.Type
type family Filename typ = result | result -> typ where
  Filename 'Dir = Const ()
  Filename 'File = Identity

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
  fmap f (List mu) = List $ firstMap f mu

instance Bifoldable XNor where
  bifoldr f g z = \case
    Neither -> z
    Both x y -> f x (g y z)

instance Bitraversable XNor where
  bitraverse f g = \case
    Neither -> pure Neither
    Both x y -> liftA2 Both (f x) (g y)

firstTraverse ::
  ( DFunctor d,
    Recursive (->) (d (f (m b))) (f (m b)),
    Steppable (->) (d (f b)) (f b),
    Bitraversable f,
    Applicative m
  ) =>
  (a -> m b) -> d (f a) -> m (d (f b))
firstTraverse f = cata (fmap embed . bisequenceA) . firstMap f

instance Traversable List where
  traverse f (List mu) = List <$> firstTraverse f mu

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

deriving stock instance
  (Traversable (Filename typ)) => Traversable (Path rel typ)

-- Tradeoffs between `Path `Any 'Pathic rep` and `Anchored (UnambiguousPath rep)`:
-- - `Anchored` involves a bunch of synonyms
-- - `Anchored` keeps `Path` disjoint
-- - `Pathic` makes parameterized types match more broadly (is there any case where we want to allow `Abs` and `Rel`, but not `Any`?)
-- - `Anchored` mokes some code more complicated (parsers)
-- - need `Anchored` and `Unambiguous` anyway, for other cases
--
-- Other thoughts:
-- - maybe `Anchored`, etc. doesn’t need all the operations, we do need a type to get us from a parameter to the disjunction of types, but that disjunction maybe doesn’t need a type … except for `NonReparented`, because it’s a subset of the available types. But, since we can’t always give the alternatives the same implementation, that requires a type class, and there’s no way to declare that a class covers a kind completely. Maybe we can do that with an @overlappable@ instance that dispatches to the two overlapping instances?

type TotalOps :: Kind.Type -> Kind.Type -> Kind.Type -> Kind.Constraint
class TotalOps dir relPath result | dir relPath -> result where
  -- | Concatenate two paths.
  --
  --   The general restriction is that the first argument must be a directory
  --   and the second must be relative. If either is reparented, the result will
  --   be reparented. If the first argument is absolute, it can’t be
  --   concatenated with a reparented path – see `</?>` for handling that case.
  --
  --  __NB__: The precedence is one higher than `System.FilePath.</>` and
  --         `Path.</>` so that cases like the one below can be written without
  --          parentheses.
  --
  --        > serialize Format.posix
  --        >   <$> [posix|/|] </?> [posix|usr/|] </> [posix|bin/|] </> [posix|env|]
  --
  --          The precedence is tricky, because `<>` also has the same precedence,
  --          but only applies to relative paths, so you can’t mix `<>` and `</>`.
  --          E.g., ideally you’d be able to write @`serialize` `<$>` abs `</?>` rel1
  --         `<>` rel2 `</>` file@ without parens, but if you could, then
  --          @`serialize` `<$>` abs `</?>` rel1 `</>` rel2 `</>` file@ would need
  --          parens either @`serialize` `<$>` (abs `</?>` rel1 `</>` rel2 `</>`
  --          file)@ or @`serialize` `<$>` abs `</?>` (rel1 `</>` rel2 `</>` file)@.
  --          Unless you can mix an associative and non-associative operator of
  --          the same precedence …
  (</>) :: dir -> relPath -> result

infixr 5 </>

instance
  TotalOps
    (Path rel 'Dir rep)
    (Path ('Rel 'False) typ rep)
    (Path rel typ rep)
  where
  parent </> child =
    Path
      { parents = parents parent,
        directories = directories child <> directories parent,
        filename = filename child
      }

instance
  TotalOps
    (Path ('Rel 'True) 'Dir rep)
    (Path ('Rel 'True) typ rep)
    (Path ('Rel 'True) typ rep)
  where
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

instance
  TotalOps
    (Path ('Rel 'False) 'Dir rep)
    (Path ('Rel 'True) typ rep)
    (Path ('Rel 'True) typ rep)
  where
  parent </> child =
    Path
      { parents =
          sum . minusNaturalMaybe (parents child) . length $ directories parent,
        directories =
          directories child <> drop (parents child) (directories parent),
        filename = filename child
      }

type PartialOps :: Kind.Type -> Kind.Type -> Kind.Type -> Kind.Constraint
class PartialOps dir relPath result | dir relPath -> result where
  -- | Concatenate a possibly-reparented path onto an absolute directory.
  --
  --  __NB__: The precedence is carefully set so that cases like the one below can
  --          be written without parentheses.
  --
  -- > :{
  --   serialize @Text Format.posix
  --     <$> [posix|/|] </?> [posix|user/|] </> [posix|../usr/bine/|] <> [posix|../bin/|] </> [posix|env|]
  -- :}
  -- Just "/usr/bin/env"
  --
  --          Note the four operators used: `<$>`, `</?>`, `</>`, and `<>`. Any
  --          change to fixity would cause this to collapse.
  (</?>) ::
    dir ->
    relPath ->
    -- | `Nothing` when the child path is reparented above the root directory.
    Maybe result

infixr 5 </?>

instance PartialOps (Path 'Abs 'Dir rep) (Path ('Rel 'True) typ rep) (Path 'Abs typ rep) where
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

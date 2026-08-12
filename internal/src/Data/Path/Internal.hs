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
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-methods:sconcat #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Path.Internal
  ( List (List),
    Parents,
    Filename,
    Path (..),
    current,
    (</>),
  )
where

--import safe "base" Data.Eq (Eq)
--import safe "base" Data.Foldable (Foldable)
--import safe "base" Data.Functor (Functor)
import safe "base" Data.Functor.Const (Const (Const))
import safe "base" Data.Functor.Identity (Identity (runIdentity))
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Monoid (Monoid, mempty)
--import safe "base" Data.Ord (Ord)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Semigroup (Semigroup, stimes, stimesMonoid, (<>))
import safe "base" GHC.Generics (Generic)
-- TODO: `minusNaturalMaybe` is exported from Numeric.Natural starting with base-4.18 (GHC 9.6).
import safe "base" Numeric.Natural (Natural)
--import safe "base" Text.Read (Read)
--import safe "base" Text.Show (Show)
--import safe "yaya" Yaya.Fold (embed)
import safe "yaya" Yaya.Pattern (Maybe (Nothing, Just))
import "this" Data.Path.Internal.List (List (List))
import "this" Data.Path.Internal.Relativity (Relativity (Abs, Any, Rel))
import "this" Data.Path.Internal.Resolution (Resolution (Res, Unres))
import "this" Data.Path.Internal.Type (Type (Dir, File))
import "this" Data.Path.Internal.Type qualified as Type (Type (Any))
import safe "base" Prelude (undefined, last, (<$>), init)

-- | The `Parents` closed type family maps `Relativity` to types that encode the
--  number of leading @../@ (or the absence thereof) in the path.
type Parents :: Relativity -> Kind.Type
type family Parents rel = result | result -> rel where
  Parents 'Abs = ()
  Parents 'Rel = Proxy Natural
  Parents 'Any = Maybe Natural

-- | The `Filename` closed type family maps each `Type` (together with a
--  generic representation type `Kind.Type`) to a type that encodes the
--  filename (or the absence thereof) in the path.
type Filename :: Type -> Kind.Type -> Kind.Type
type family Filename typ = result | result -> typ where
  Filename 'Dir = Const ()
  Filename 'File = Identity
  Filename 'Type.Any = Maybe

type Directories :: Resolution -> Relativity -> Kind.Type -> Kind.Type
type family Directories res rel rep = result | result -> rep rel res where
  -- XXX: we assume that all lists here have directories from deep to shallow
  -- going from left to right
  Directories 'Res   'Rel rep = Maybe (rep, [Maybe rep])
  Directories 'Res   'Abs rep = Identity [rep]
  Directories 'Unres 'Rel rep = [Maybe rep]
  Directories 'Unres 'Abs rep = (rep, [Maybe rep])


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
type Path :: Resolution -> Relativity -> Type -> Kind.Type -> Kind.Type
data Path res rel typ rep = Path
  { parents :: Parents rel,
    directories :: Directories res rel rep,
    filename :: Filename typ rep
  }
  deriving stock (Generic)

--deriving stock instance
--  (Eq (Parents rel), Eq (Filename typ rep), Eq rep) => Eq (Path res rel typ rep)
--
--deriving stock instance
--  (Ord (Parents rel), Ord (Filename typ rep), Ord rep) => Ord (Path res rel typ rep)
--
--deriving stock instance
--  (Read (Parents rel), Read (Filename typ rep), Read rep) =>
--  Read (Path res rel typ rep)

--deriving stock instance
--  (Show (Parents rel), Show (Filename typ rep), Show rep) =>
--  Show (Path res rel typ rep)

--deriving stock instance
--  (Foldable (Filename typ)) => Foldable (Path res rel typ)
--
--deriving stock instance
--  (Functor (Filename typ)) => Functor (Path res rel typ)
--
--deriving stock instance
--  (Traversable (Filename typ)) => Traversable (Path res rel typ)

-- | Type-level ternary function to compute TODO. add doc
-- TODO: also `Min` needs to change name.
type Min :: (Resolution, Relativity) -> Resolution -> Resolution
type family Min a b where
  Min '( 'Res, 'Rel) 'Res = 'Res
  Min _ _ = 'Unres

-- | TODO: update doc
type TotalOps :: Resolution -> Relativity -> Resolution -> Kind.Constraint
class TotalOps res rel res' where
  (<<>>) :: Directories res' 'Rel rep -> Directories res rel rep -> Directories (Min '(res, rel) res') rel rep

-- | Concatenate two paths.  TODO: update doc
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
(</>) :: TotalOps res rel res' => Path res rel 'Dir rep -> Path res' 'Rel typ rep -> Path (Min '(res, rel) res') rel typ rep
parent </> child =
  Path
    { parents = undefined,
      directories = directories child <<>> directories parent,
      filename = filename child
    }

infixr 5 </>

instance TotalOps 'Res 'Rel 'Res where
  child <<>> Nothing = child
  Nothing <<>> parent = parent
  Just (c, cs) <<>> Just (p, ps) = Just (p, cs <> (Just c:ps))

instance TotalOps 'Res 'Rel 'Unres where
  child <<>> Nothing = child
  child <<>> Just (p, ps) = child <> ps <> [Just p]

instance TotalOps 'Res 'Abs 'Res where
  Nothing <<>> parent = (last parent', Just <$> init (parent'))
    where parent' = runIdentity parent
  Just (c, cs) <<>> parent = (last parent', cs <> (Just <$> init (c:parent')))
    where parent' = runIdentity parent

instance TotalOps 'Res 'Abs 'Unres where
  child <<>> parent = (last parent', child <> (Just <$> init parent'))
    where parent' = runIdentity parent

instance TotalOps 'Res 'Any 'Res where
  child <<>> parent = undefined -- TODO

instance TotalOps 'Res 'Any 'Unres where
  child <<>> parent = undefined -- TODO

instance TotalOps 'Unres 'Rel 'Res where
  Nothing <<>> parent = parent
  Just (c, cs) <<>> parent = cs <> (Just c:parent)

instance TotalOps 'Unres 'Rel 'Unres where
  (<<>>) = (<>)

instance TotalOps 'Unres 'Abs 'Res where
  Nothing <<>> parents = parents
  Just (c, cs) <<>> (p, ps) = (p, cs <> (Just c:ps))

instance TotalOps 'Unres 'Abs 'Unres where
  child <<>> (p, ps) = (p, child <> ps)

instance TotalOps 'Unres 'Any 'Res where
  child <<>> parent = undefined -- TODO

instance TotalOps 'Unres 'Any 'Unres where
  child <<>> parent = undefined -- TODO

-- | This does /not/ represent the “current directory” as in the absolute path
--   from which the program was run or something, but rather the path “./”.
current :: Path 'Res 'Rel 'Dir rep
current =
  Path {parents = Proxy, directories = Nothing, filename = Const ()}

-- | Forget that a relative path doesn’t have any parents.
weaken :: Path 'Res 'Rel typ rep -> Path 'Unres 'Rel typ rep
weaken Path{..} =
  Path { parents = undefined,
         directories = case directories of
                          Nothing -> []
                          Just (a, as) -> as <> [Just a]
       , filename = filename }

-- unsafeResolve :: Path 'Unres rel typ rep -> Maybe (Path 'Res rel typ rep)

-- | Only relative directories form a semigroup under `</>`.
instance Semigroup (Path 'Res 'Rel 'Dir rep) where
  (<>) = (</>)
  stimes = stimesMonoid

-- | Only relative directories form a semigroup under `</>`.
instance Semigroup (Path 'Unres 'Rel 'Dir rep) where
  (<>) = (</>)
  stimes = stimesMonoid

---- | Only relative directories form a monoid under concatenation.
instance Monoid (Path 'Res 'Rel 'Dir rep) where
  mempty = current

-- | Only relative directories form a monoid under concatenation.
instance Monoid (Path 'Unres 'Rel 'Dir rep) where
  mempty = weaken current

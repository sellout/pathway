{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- "GHC.Natural" is ‘Unsafe’ before base 4.14.4. We can’t conditionalize the
-- Safe Haskell extension (because it forces Safe Haskell-using consumers to
-- conditionalize), so this silences the fact that this module is inferred
-- ‘Safe’ in some configurations. We should be able to remove this (and this
-- module made ‘Safe’) once base-4.14.4 is the oldest supported version.
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
--   A representation-agnostic, structured, type-safe path library.
--
--  __NOTE__: This library stores paths in a normalized form. However, because
--            it’s a pure library, it is not canonicalized. This makes the
--            behavior consistent across systems, whereas with canonical paths,
--            `/a/b/../c` could behave differently between Posix and Windows. If
--            `/a/b/` is a symlink to /d/e/`, then Posix would canonicalize the
--            path to `/d/c`, whereas Windows would canonicalize to `/a/c`. If
--            you want system-specific canonicalization, look at the
--            @pathway-system@ package, which depends on this one.
--
-- @since 99999
module Data.Path
  ( Anchored (..),
    AnyPath,
    Filename,
    Parents,
    Path,
    Pathish (..),
    Pathy,
    Prefixed (..),
    RelOps (..),
    Relative,
    Relativity (Abs, Rel),
    Substible,
    Type (Dir, File),
    Typey,
    anchor,
    current,
    escapeComponent,
    forgetRelativity,
    forgetType,
    minimalRoute,
    reparent,
    replace,
    route,
    strengthen,
    toText,
    unanchor,
    weaken,
    (</>),
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category (id, (.))
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Foldable (Foldable)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor, fmap)
import safe "base" Data.Functor.Const (Const (Const))
import safe "base" Data.Functor.Identity (runIdentity)
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Maybe qualified as Lazy
import safe "base" Data.Monoid (Monoid, mempty)
import safe "base" Data.Ord (Ord)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Traversable (Traversable)
import safe "base" GHC.Generics (Generic, Generic1)
-- TODO: `minusNaturalMaybe` is exported from Numeric.Natural starting with base-4.18 (GHC 9.6).
import "base" GHC.Natural (minusNaturalMaybe)
import safe "base" Numeric.Natural (Natural)
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show)
import safe "extra" Data.List.Extra qualified as List
import safe "pathway-internal" Data.Path.Internal
  ( Filename,
    List,
    Parents,
    Path (Path),
    current,
    directories,
    filename,
    parents,
    weaken,
    (</>),
  )
import safe "text" Data.Text (Text)
import safe "text" Data.Text qualified as Text
import safe "yaya" Yaya.Applied (append, length, reverse, tail)
import safe "yaya" Yaya.Fold
  ( Projectable,
    Recursive,
    Steppable,
    cata,
    distTuple,
    embed,
    gcata,
    project,
  )
import safe "yaya" Yaya.Fold.Native ()
import safe "yaya" Yaya.Pattern
  ( Maybe (Nothing),
    Pair ((:!:)),
    XNor (Both, Neither),
    fromMaybe,
    fst,
    isJust,
    maybe,
    xnor,
  )
import safe "yaya-containers" Yaya.Containers.Pattern.Map (MapF (BinF, TipF))
import safe "this" Data.Path.Format (Format, parent, root, separator, substitutions)
import safe "this" Data.Path.Relativity (Relativity (Abs, Any, Rel))
import safe "this" Data.Path.Type (Type (Dir, File))
import safe "this" Data.Path.Type qualified as Type (Type (Any))
import safe "base" Prelude ((+))

-- $setup
-- >>> :seti -XQuasiQuotes
-- >>> :seti -XTypeApplications
-- >>> import Data.Path.Directory ((</?>))
-- >>> import qualified Data.Path.Format as Format
-- >>> import Data.Path.TH (posix)
-- >>> import "QuickCheck" Test.QuickCheck
-- >>> import "base" Data.Bool (Bool (True))
-- >>> import "base" Data.Functor ((<$>))
-- >>> import "pathway-quickcheck" Test.Path.QuickCheck ()
-- >>> import "yaya" Yaya.Pattern (fromJust, isNothing)

-- | __FIXME__: Move upstream.
isNeither :: XNor a b -> Bool
isNeither = xnor True (\_ _ -> False)

null :: (Projectable (->) t (XNor a)) => t -> Bool
null = isNeither . project

-- | Convert an arbitrary path value to a specific path type.
--
-- @since 99999
type Pathish :: Kind.Type -> Relativity -> Type -> Kind.Type -> Kind.Constraint
class Pathish path (rel :: Relativity) (typ :: Type) rep where
  -- | Convert to the requested path type.
  --
  -- @since 99999
  specializePath :: path -> Path rel typ rep

-- | This does not convert `AnyPath` to a more specific `Path` type (see
--  `anchor` for that), but converts some path representation to a `Path`. So
--   for `Path` itself, this is always `id`.
--
--  __NB__: This is the only instance needed by Pathway itself, but the class
--          exists as an integration point for other libraries that have some
--          typed path representation.
--
-- @since 99999
instance Pathish (Path rel typ rep) rel typ rep where
  specializePath = id

-- | This is a path where we don’t know (at the type level) whether it’s
--   anchored, or whether it’s a file. `parents` is `None` for an absolute path
--   and otherwise contains the number of leading `../` parents that should be
--   produced.
--
--   There are not many operations on this type, but it is the type used for
--   generic operations like parsing and printing, with separate functions (like
--  `anchor`) to lift the contained information to the type level.
--
-- @since 99999
type AnyPath :: Kind.Type -> Kind.Type
type AnyPath = Path 'Any 'Type.Any

-- -- | Convert an arbitrary value representing a path to `AnyPath`.
-- --
-- --  __NB__: Don’t make an instance for `FilePath`, as it would be partial. Not
-- --          all strings represent paths. Instead, use the parsers that exist for
-- --          that purpose.
-- class Pathy path rep where
--   generalizePath :: path -> AnyPath rep

-- | The `Relativity`s whose paths can be generalized to `Any` relativity.
--
-- @since 99999
type Relative :: Relativity -> Kind.Constraint
class Relative rel where
  -- | Forget the relativity of the leading @../@ components.
  --
  -- @since 99999
  generalizeRelativity :: Parents rel -> Parents 'Any

-- | @since 99999
instance Relative 'Abs where
  generalizeRelativity () = Nothing

-- | @since 99999
instance Relative 'Any where
  generalizeRelativity = id

-- | @since 99999
instance Relative ('Rel 'False) where
  generalizeRelativity Proxy = pure 0

-- | @since 99999
instance Relative ('Rel 'True) where
  generalizeRelativity = pure

-- | The `Type`s whose paths can be generalized to `Type.Any` type.
--
-- @since 99999
type Typey :: Type -> Kind.Constraint
class Typey typ where
  -- | Forget whether the final component is a directory or a file.
  --
  -- @since 99999
  generalizeType :: Filename typ rep -> Filename 'Type.Any rep

-- | @since 99999
instance Typey 'Dir where
  generalizeType (Const ()) = Nothing

-- | @since 99999
instance Typey 'File where
  generalizeType = pure . runIdentity

-- | @since 99999
instance Typey 'Type.Any where
  generalizeType = id

-- | The paths that can be generalized to an `AnyPath`.
--
-- @since 99999
type Pathy :: Relativity -> Type -> Kind.Constraint
type Pathy rel typ = (Relative rel, Typey typ)

-- -- | This is the only instance needed by Pathway itself, but the class exists as
-- --   an integration point for other libraries that have some path
-- --   representation.
-- instance (Relative rel, Typey typ) => Pathy (Path rel typ rep) rep where
--   generalizePath path =
--     Path
--       { parents = generalizeRelativity $ parents path,
--         directories = directories path,
--         filename = generalizeType $ filename path
--       }

-- defaultGeneralizePath ::
--   forall path rel typ rep.
--   (Pathish path rel typ rep, Pathy (Path rel typ rep) rep) =>
--   Proxy rel ->
--   Proxy typ ->
--   path ->
--   AnyPath rep
-- defaultGeneralizePath Proxy Proxy =
--   generalizePath @(Path rel typ rep) . specializePath

-- | Operations on relative paths.
--
-- @since 99999
type RelOps :: Relativity -> Kind.Constraint
class RelOps rel where
  -- | The parent of a relative directory. This removes the innermost directory
  --   if there is one, and otherwise adds a leading @../@.
  --
  -- @since 99999
  ascendRelative :: Path rel 'Dir rep -> Path ('Rel 'True) 'Dir rep

  -- | Add the given number of leading @../@ components.
  --
  -- @since 99999
  reparentBy :: Natural -> Path rel typ rep -> Path ('Rel 'True) typ rep

  -- | Like `route`, but for reparented paths. Unlike `route`, this
  --   can fail. If the first argument is reparented more than the second
  --   argument, then it returns `Nothing`. This is because the result requires
  --   directory names that we don’t have available. For example:
  --
  -- -- prop> maybe True ((b ==) . (a </>)) $ maybeRoute @_ @Text @'File a b
  --
  -- >>> maybeRoute @('Rel 'True) @Text [posix|../../d/e/|] [posix|../f/g/|]
  -- Nothing
  --
  --   we can see why if we first convert them to absolute paths
  --
  -- >>> let from = fromJust $ [posix|/a/b/c/|] </?> [posix|../../d/e/|]
  -- >>> toText @_ @_ @Text Format.posix from
  -- "/a/d/e/"
  -- >>> let to = fromJust $ [posix|/a/b/c/|] </?> [posix|../f/g/|]
  -- >>> toText @_ @_ @Text Format.posix to
  -- "/a/b/f/g/"
  -- >>> toText @_ @_ @Text Format.posix $ minimalRoute from to
  -- "../../b/f/g/"
  --
  --   You can see that the directory “b” is in the result, but isn’t present in
  --   the original relative paths, so there is no way to produce the desired
  --   result without adding more context to the arguments.
  --
  --   In support of the example above, here's a ASCII art diagram representing
  --   the directory structure relative to the example above, with "?" being a
  --   directory which we don't know the name of, and "cwd" being the current
  --   working directory:
  --
  --   >       ?
  --   > ┌─────┤
  --   > d     ?
  --   > │     ├─────┐
  --   > e    cwd    f
  --   >             │
  --   >             g
  --
  --   From the diagram, the issue of rerouting should be clearer:
  --
  -- - rerouting from @g@ to @e@ (or, more accurately, from @..\/f\/g\/@ to
  --   @..\/..\/d\/e\/@) requires going /up from/ an unknown directory, which
  --   is done via @..@, that is not needing the name of such unknown
  --   directory;
  -- - going from @e@ to @g@, instead, entails going /down into/ a directory,
  --   which obviously requires knowing the name of such a directory.
  --
  -- @since 99999
  maybeRoute ::
    Path ('Rel 'True) 'Dir rep ->
    Path rel typ rep ->
    Maybe (Path ('Rel 'True) typ rep)

  -- | Like `maybeRoute`, but shortens the path as much as possible. This can
  --   only make a difference when both paths have the same amount of
  --   reparenting. E.g.
  --
  -- >>> toText Format.posix <$> maybeMinimalRoute @('Rel 'True) @Text [posix|../d/e/|] [posix|../d/f/|]
  -- Just "../f/"
  --
  --   whereas
  --
  -- >>> toText Format.posix <$> maybeRoute @('Rel 'True) @Text [posix|../d/e/|] [posix|../d/f/|]
  -- Just "../../d/f/"
  --
  --   The thing to consider is that various operations can fail depending on how
  --   much reparenting is in a path. In some cases (the second argument to
  --   `maybeRoute`) more is safer, while in others (concatenating onto an
  --   absolute directory) less is safer. Carefully choosing and ordering path
  --   operations to minimize the partiality is worthwhile.
  --
  -- @since 99999
  maybeMinimalRoute ::
    (Eq rep) =>
    Path ('Rel 'True) 'Dir rep ->
    Path rel typ rep ->
    Maybe (Path ('Rel 'True) typ rep)

-- | @since 99999
instance RelOps ('Rel 'False) where
  ascendRelative dir =
    if null $ directories dir
      then dir {parents = 1}
      else dir {parents = 0, directories = tail $ directories dir}
  reparentBy levels path = path {parents = levels}
  maybeRoute from to =
    Lazy.maybe
      Nothing
      ( \diffParents ->
          pure $
            Path
              { parents = length (directories from) + diffParents,
                directories = directories to,
                filename = filename to
              }
      )
      $ minusNaturalMaybe 0 (parents from)
  maybeMinimalRoute ::
    forall typ rep.
    (Eq rep) =>
    Path ('Rel 'True) 'Dir rep ->
    Path ('Rel 'False) typ rep ->
    Maybe (Path ('Rel 'True) typ rep)
  maybeMinimalRoute from to =
    if parents from == 0
      then
        let (_, (newFrom, newTo)) :: (List rep, (List rep, List rep)) =
              partitionCommonPrefix (reverse $ directories from) . reverse $
                directories to
         in pure $
              Path
                { parents = length newFrom,
                  directories = reverse newTo,
                  filename = filename to
                }
      else Nothing

-- | @since 99999
instance RelOps ('Rel 'True) where
  ascendRelative dir =
    if null $ directories dir
      then dir {parents = parents dir + 1}
      else dir {directories = tail $ directories dir}
  reparentBy levels path = path {parents = parents path + levels}
  maybeRoute from to =
    Lazy.maybe
      Nothing
      ( \diffParents ->
          pure $
            Path
              { parents = length (directories from) + diffParents,
                directories = directories to,
                filename = filename to
              }
      )
      $ minusNaturalMaybe (parents to) (parents from)
  maybeMinimalRoute ::
    forall typ rep.
    (Eq rep) =>
    Path ('Rel 'True) 'Dir rep ->
    Path ('Rel 'True) typ rep ->
    Maybe (Path ('Rel 'True) typ rep)
  maybeMinimalRoute from to =
    if parents from == parents to
      then
        let (_, (newFrom, newTo)) :: (List rep, (List rep, List rep)) =
              partitionCommonPrefix (reverse $ directories from) . reverse $
                directories to
         in pure $
              Path
                { parents = length newFrom,
                  directories = reverse newTo,
                  filename = filename to
                }
      else maybeRoute from to

-- | Add a single leading @../@.
--
-- @since 99999
reparent :: (RelOps rel) => Path rel typ rep -> Path ('Rel 'True) typ rep
reparent = reparentBy 1

partitionCommonPrefix' ::
  ( Eq a,
    Steppable (->) t (XNor a),
    Steppable (->) u (XNor a),
    Recursive (->) prefix (XNor a),
    Steppable (->) prefix (XNor a)
  ) =>
  XNor a (Pair t (u -> (prefix, (t, u)))) ->
  u ->
  (prefix, (t, u))
partitionCommonPrefix' (Both x (_ :!: fn)) (project -> Both y u) =
  let (prefix, (one, two)) = fn u
   in if x == y
        then (embed $ Both x prefix, (one, two))
        else
          ( embed Neither,
            ( embed . Both x $ append prefix one,
              embed . Both y $ append prefix two
            )
          )
partitionCommonPrefix' one two = (embed Neither, (embed (fmap fst one), two))

-- | Given two lists, returns any common prefix as well as the two distinct
--   tails.
partitionCommonPrefix ::
  ( Eq a,
    Recursive (->) t (XNor a),
    Steppable (->) t (XNor a),
    Steppable (->) u (XNor a),
    Recursive (->) prefix (XNor a),
    Steppable (->) prefix (XNor a)
  ) =>
  t ->
  u ->
  (prefix, (t, u))
partitionCommonPrefix =
  gcata (distTuple embed) partitionCommonPrefix'

type Routable :: Relativity -> Relativity -> Kind.Constraint
class Routable from to where
  -- | Creates a path relative to the first argument that that points to the
  --   same location as the second argument.
  --
  -- @since 99999
  route ::
    Path from 'Dir rep -> Path to typ rep -> Path ('Rel 'True) typ rep

  -- | Creates a path relative to the first argument that that points to the
  --   same location as the second argument.
  --
  -- @since 99999
  minimalRoute ::
    (Eq rep) =>
    Path from 'Dir rep ->
    Path to typ rep ->
    Path ('Rel 'True) typ rep

-- |
--
-- prop> maybe True (b ==) $ a </?> route  @_ @_ @Text @'File a b
--
-- @since 99999
instance Routable 'Abs 'Abs where
  route ::
    forall typ rep.
    Path 'Abs 'Dir rep ->
    Path 'Abs typ rep ->
    Path ('Rel 'True) typ rep
  route from to =
    Path
      { parents = length $ directories from,
        directories = directories to,
        filename = filename to
      }
  minimalRoute ::
    forall typ rep.
    (Eq rep) =>
    Path 'Abs 'Dir rep ->
    Path 'Abs typ rep ->
    Path ('Rel 'True) typ rep
  minimalRoute from to =
    let (_, (newFrom, newTo)) :: (List rep, (List rep, List rep)) =
          partitionCommonPrefix (reverse $ directories from) . reverse $
            directories to
     in Path
          { parents = length newFrom,
            directories = reverse newTo,
            filename = filename to
          }

-- | @since 99999
instance Routable ('Rel 'False) ('Rel 'True) where
  route from to =
    Path
      { parents = length (directories from) + parents to,
        directories = directories to,
        filename = filename to
      }
  minimalRoute ::
    forall typ rep.
    (Eq rep) =>
    Path ('Rel 'False) 'Dir rep ->
    Path ('Rel 'True) typ rep ->
    Path ('Rel 'True) typ rep
  minimalRoute from to =
    let (_, (newFrom, newTo)) :: (List rep, (List rep, List rep)) =
          partitionCommonPrefix (reverse $ directories from) . reverse $
            directories to
     in Path
          { parents = length newFrom,
            directories = reverse newTo,
            filename = filename to
          }

-- | @since 99999
instance Routable ('Rel 'False) ('Rel 'False) where
  route from to =
    Path
      { parents = length $ directories from,
        directories = directories to,
        filename = filename to
      }
  minimalRoute ::
    forall typ rep.
    (Eq rep) =>
    Path ('Rel 'False) 'Dir rep ->
    Path ('Rel 'False) typ rep ->
    Path ('Rel 'True) typ rep
  minimalRoute from to =
    let (_, (newFrom, newTo)) :: (List rep, (List rep, List rep)) =
          partitionCommonPrefix (reverse $ directories from) . reverse $
            directories to
     in Path
          { parents = length newFrom,
            directories = reverse newTo,
            filename = filename to
          }

-- | This is like `minimalRoute`, but trades off weakening the path for the
--   possibility of failure.
--
--   This is useful for sandboxing, as anything outside of the first path will
--   result in `Nothing`, whereas `minimalRoute` can return valid paths outside
--   of the first argument.
--
-- -- prop> isNothing (maybeMinimalRoute a b) ==> isNothing (routePrefix a b)
--
-- @since 99999
type Prefixed :: Relativity -> Relativity -> Kind.Constraint
class Prefixed rel rel' | rel -> rel' where
  -- | Returns `False` exactly when `routePrefix` would return `Nothing`.
  --
  -- -- prop> isPrefix a b == not (isNothing $ routePrefix a b)
  --
  -- @since 99999
  isPrefix :: (Eq rep) => Path rel 'Dir rep -> Path rel typ rep -> Bool
  isPrefix a = isJust . routePrefix a

  -- | If the first argument is a prefix of the second argument, it will remove
  --   the prefix, returning a relative path. If it’s not a prefix, it returns
  --  `Nothing`.
  --
  -- @since 99999
  routePrefix ::
    (Eq rep) =>
    Path rel 'Dir rep ->
    Path rel typ rep ->
    Maybe (Path rel' typ rep)

-- | @since 99999
instance Prefixed 'Abs ('Rel 'False) where
  routePrefix ::
    forall typ rep.
    (Eq rep) =>
    Path 'Abs 'Dir rep ->
    Path 'Abs typ rep ->
    Maybe (Path ('Rel 'False) typ rep)
  routePrefix from to =
    let (_, (a, b)) :: (List rep, (List rep, List rep)) =
          partitionCommonPrefix (reverse $ directories from) . reverse $
            directories to
     in if null a
          then
            pure
              Path
                { parents = Proxy,
                  directories = reverse b,
                  filename = filename to
                }
          else Nothing

-- | @since 99999
instance Prefixed ('Rel 'False) ('Rel 'False) where
  routePrefix ::
    forall typ rep.
    (Eq rep) =>
    Path ('Rel 'False) 'Dir rep ->
    Path ('Rel 'False) typ rep ->
    Maybe (Path ('Rel 'False) typ rep)
  routePrefix from to =
    let (_, (a, b)) :: (List rep, (List rep, List rep)) =
          partitionCommonPrefix (reverse $ directories from) . reverse $
            directories to
     in if null a
          then
            pure
              Path
                { parents = Proxy,
                  directories = reverse b,
                  filename = filename to
                }
          else Nothing

-- | This instance likely isn’t useful directly, as it will simply fail more
--   often than `maybeRoute`, with no improvement in the type.
--
-- @since 99999
instance Prefixed ('Rel 'True) ('Rel 'True) where
  routePrefix ::
    forall typ rep.
    (Eq rep) =>
    Path ('Rel 'True) 'Dir rep ->
    Path ('Rel 'True) typ rep ->
    Maybe (Path ('Rel 'True) typ rep)
  routePrefix from to =
    if parents from == parents to
      then
        let (_, (a, b)) :: (List rep, (List rep, List rep)) =
              partitionCommonPrefix (reverse $ directories from) . reverse $
                directories to
         in if null a
              then
                pure
                  Path
                    { parents = parents to,
                      directories = reverse b,
                      filename = filename to
                    }
              else Nothing
      else Nothing

-- | Representations that support substitution, which is how a `Format` escapes
--   path components.
--
-- @since 99999
type Substible :: Kind.Type -> Kind.Constraint
class Substible a where
  -- | @`replace` needle replacement haystack@ replaces every occurrence of
  --   @needle@ in @haystack@ with @replacement@.
  --
  -- @since 99999
  replace :: a -> a -> a -> a

-- | @since 99999
instance Substible String where
  replace = List.replace

-- | @since 99999
instance Substible Text where
  replace = Text.replace

escape' :: (Substible a) => MapF a a (a -> a) -> a -> a
escape' = \case
  TipF -> id
  BinF _ direct escaped fn fn' -> fn' . replace direct escaped . fn
{-# INLINEABLE escape' #-}

-- | Apply the `substitutions` of a `Format` to a single path component.
--
-- @since 99999
escapeComponent :: (Substible rep) => Format rep -> rep -> rep
escapeComponent = cata escape' . substitutions
{-# INLINEABLE escapeComponent #-}

anyToText :: (Monoid a, Substible a) => Format a -> AnyPath a -> a
anyToText format path =
  let escapeComponent' = escapeComponent format
      prefix =
        maybe
          (root format)
          ( fromMaybe mempty
              . cata (fmap $ maybe (parent format) (parent format <>))
          )
          $ parents path
      directory =
        cata
          ( \case
              Neither -> mempty
              Both directoryName pathStr ->
                pathStr <> escapeComponent' directoryName <> separator format
          )
          $ directories path
      file = maybe mempty escapeComponent' $ filename path
   in prefix <> directory <> file
{-# INLINEABLE anyToText #-}

-- |
--
-- >>> toText Format.posix [posix|.be/|]
-- ".be/"
--
-- @since 99999
toText ::
  (Pathy rel typ, Monoid a, Substible a) => Format a -> Path rel typ a -> a
toText format = anyToText format . unanchor
{-# INLINEABLE toText #-}

-- | Recover a relative path that has no leading @../@ components, the inverse
--   of `weaken`.
--
--  __TODO__: This forms a `Prism'` with `weaken`.
--
-- @since 99999
strengthen :: Path ('Rel 'True) typ rep -> Maybe (Path ('Rel 'False) typ rep)
strengthen path =
  if parents path == 0 then pure path {parents = Proxy} else Nothing

-- | A path of any specific type, with the type recovered as a constructor.
--
-- @since 99999
type Anchored :: Kind.Type -> Kind.Type
data Anchored rep
  = -- | An absolute directory.
    --
    -- @since 99999
    AbsDir (Path 'Abs 'Dir rep)
  | -- | An absolute file.
    --
    -- @since 99999
    AbsFile (Path 'Abs 'File rep)
  | -- | A relative directory with no leading @../@.
    --
    -- @since 99999
    RelDir (Path ('Rel 'False) 'Dir rep)
  | -- | A relative file with no leading @../@.
    --
    -- @since 99999
    RelFile (Path ('Rel 'False) 'File rep)
  | -- | A relative directory that may have leading @../@.
    --
    -- @since 99999
    ReparentedDir (Path ('Rel 'True) 'Dir rep)
  | -- | A relative file that may have leading @../@.
    --
    -- @since 99999
    ReparentedFile (Path ('Rel 'True) 'File rep)
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Ord,
      -- | @since 99999
      Read,
      -- | @since 99999
      Show
    )
  deriving stock
    ( -- | @since 99999
      Foldable,
      -- | @since 99999
      Functor,
      -- | @since 99999
      Generic1,
      -- | @since 99999
      Traversable
    )

type role Anchored nominal

-- | Discover the specific type of a `Path`.
--
-- @since 99999
anchor :: AnyPath rep -> Anchored rep
anchor path =
  maybe
    ( maybe
        ( AbsDir
            Path
              { parents = (),
                directories = directories path,
                filename = Const ()
              }
        )
        ( \filenm ->
            AbsFile
              Path
                { parents = (),
                  directories = directories path,
                  filename = pure filenm
                }
        )
        $ filename path
    )
    ( \case
        0 ->
          maybe
            ( RelDir
                Path
                  { parents = Proxy,
                    directories = directories path,
                    filename = Const ()
                  }
            )
            ( \filenm ->
                RelFile
                  Path
                    { parents = Proxy,
                      directories = directories path,
                      filename = pure filenm
                    }
            )
            $ filename path
        parnts ->
          maybe
            ( ReparentedDir
                Path
                  { parents = parnts,
                    directories = directories path,
                    filename = Const ()
                  }
            )
            ( \filenm ->
                ReparentedFile
                  Path
                    { parents = parnts,
                      directories = directories path,
                      filename = pure filenm
                    }
            )
            $ filename path
    )
    $ parents path

-- | Forget the specific `Relativity` of the path.
--
-- @since 99999
forgetRelativity :: (Relative rel) => Path rel typ rep -> Path 'Any typ rep
forgetRelativity path = path {parents = generalizeRelativity $ parents path}

-- | Forget the specific `Type` of the path.
--
-- @since 99999
forgetType :: (Typey typ) => Path rel typ rep -> Path rel 'Type.Any rep
forgetType path = path {filename = generalizeType $ filename path}

-- | Forget the specific type of the path (which can be recovered with
--  `anchor`).
--
-- @since 99999
unanchor :: (Pathy rel typ) => Path rel typ rep -> AnyPath rep
unanchor = forgetRelativity . forgetType

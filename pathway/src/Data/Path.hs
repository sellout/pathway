{-# LANGUAGE OverloadedStrings #-}
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
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- A representation-agnostic, structured, type-safe path library.
--
-- __NB__: This library stores paths in a normalized form. However, because it
--         is a pure library, it is not canonicalized. This makes the behavior
--         consistent across systems, whereas with canonical paths, `/a/b/../c`
--         could behave differently between Posix and Windows. If `/a/b/` is a
--         symlink to /d/e/`, then Posix would canonicalize the path to `/d/c`,
--         whereas Windows would canonicalize to `/a/c`. If you want
--         system-specific canonicalization, look at the @pathway-system@
--         package, which depends on this one. Also, regardless of system,
--         @canonicalize parent </> child@ has different semantics than
--         @canonicalize (parent </> child)@. Given parent = /a/b/ and child =
--         ../c, on POSIX, the first one would result in /d/c (since the symlink
--         is followed before the `../` is normalized) and the second would
--         result in /a/c (because the `../` is normalized before we see that
--         `/a/b/` is a symlink).
--
-- __TODO__: Many of the path types involve jumping through hoops with `Compose`
--           and `Flip` in order to avoid more newtypes. This isn’t great, but
--           it does make things very generic – for example, we can provide
--           instances for our classes over things ilke Chris Penner’s ‘path’
--           library, making for easier adoption of parts of this approach.
module Data.Path
  ( Path,
    Pathish (..),
    Pathy,
    Prefixed (..),
    RelOps (..),
    Relative,
    Relativity (..),
    Type (..),
    Typey,
    Filename,
    Flip (..),
    Flip1 (..),
    current,
    forgetRelativity,
    forgetType,
    minimalRoute,
    reparent,
    route,
    strengthen,
    unanchor,
    (</>),
    disambiguate,
    liftRelativity,
    extractType,
    liftType,
    lift,
    serialize,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category (id, (.))
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either (Left))
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Function (const, ($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.Functor.Compose (Compose (Compose), getCompose)
import safe "base" Data.Functor.Const (Const (Const))
import safe "base" Data.Functor.Identity (Identity (Identity), runIdentity)
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Maybe qualified as Lazy
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Semigroup (Semigroup, (<>))
import safe "base" Data.String (IsString, String)
-- TODO: `minusNaturalMaybe` is exported from Numeric.Natural starting with base-4.18 (GHC 9.6).
import "base" GHC.Natural (minusNaturalMaybe)
import safe "base" Numeric.Natural (Natural)
import safe "extra" Data.List.Extra qualified as List
import safe "pathway-internal" Data.Path.Internal
  ( Filename,
    List,
    Path (Path),
    Relativity (Abs, Rel),
    Type (Dir, File),
    current,
    directories,
    filename,
    parents,
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
import safe "this" Data.Path.Ambiguous qualified as Ambiguous
import safe "this" Data.Path.Anchored (anchored)
import safe "this" Data.Path.Anchored qualified as Anchored
import safe "this" Data.Path.Any qualified as Any
import safe "this" Data.Path.Format (Format, parent, root, separator, substitutions)
import safe "this" Data.Path.Functor
  ( Flip (Flip),
    Flip1 (Flip1),
    dmap,
    dtraverse,
    unflip,
    unflip1,
  )
import safe "this" Data.Path.Unambiguous (Unambiguous, unambiguous)
import safe "this" Data.Path.Unambiguous qualified as Unambiguous
import safe "base" Prelude ((+))

-- $setup
-- >>> :seti -XQuasiQuotes
-- >>> :seti -XTypeApplications
-- >>> :seti -XNoOverloadedStrings
-- >>> import "QuickCheck" Test.QuickCheck
-- >>> import "base" Data.Bool (Bool (True))
-- >>> import "base" Data.Functor ((<$>))
-- >>> import "pathway-quickcheck" Test.Path.QuickCheck ()
-- >>> import "yaya" Yaya.Pattern (fromJust, isNothing)
-- >>> import "this" Data.Path.Directory ((</?>))
-- >>> import qualified "this" Data.Path.Format as Format
-- >>> import "this" Data.Path.TH (posix)

-- | __FIXME__: Move upstream.
isNeither :: XNor a b -> Bool
isNeither = xnor True (\_ _ -> False)

null :: (Projectable (->) t (XNor a)) => t -> Bool
null = isNeither . project

-- | Convert an arbitrary path value to a specific path type.
type Pathish :: Kind.Type -> Relativity -> Type -> Kind.Type -> Kind.Constraint
class Pathish path (rel :: Relativity) (typ :: Type) rep where
  specializePath :: path -> Path rel typ rep

-- | This does not convert `AnyPath` to a more specific `Path` type (see
--  `anchor` for that), but converts some path representation to a `Path`. So
--   for `Path` itself, this is always `id`.
--
--  __NB__: This is the only instance needed by Pathway itself, but the class
--          exists as an integration point for other libraries that have some
--          typed path representation.
instance Pathish (Path rel typ rep) rel typ rep where
  specializePath = id

type Relative :: Relativity -> Kind.Constraint
class Relative rel where
  -- | Returns the number of reparentings in this path (or `Nothing` for an absolute path).
  mayParents :: Path rel typ rep -> Maybe Natural

  forgetRelativity :: Path rel typ rep -> Anchored.Path typ rep

  -- | Lift the `Type` to the type level. If it doesn’t have the specific
  --   `Relativity`, it fails with a user-provided error parameterized over the
  --   encountered `Relativity`.
  liftRelativity :: (Relativity -> e) -> Any.Path rep -> Either e (Unambiguous.Path rel rep)

instance Relative 'Abs where
  mayParents Path {} = Nothing
  forgetRelativity = Compose . Anchored.Absolute . Flip . Flip1
  liftRelativity err =
    anchored
      (pure . Compose . dmap unflip1 . getCompose . getCompose)
      (const . Left . err $ Rel False)
      (const . Left . err $ Rel True)

instance Relative ('Rel 'False) where
  mayParents Path {} = pure 0
  forgetRelativity = Compose . Anchored.Relative . Flip . Flip1
  liftRelativity err =
    anchored
      (const . Left . err $ Abs)
      (pure . Compose . dmap unflip1 . getCompose . getCompose)
      (const . Left . err $ Rel True)

instance Relative ('Rel 'True) where
  mayParents Path {parents} = pure parents
  forgetRelativity = Compose . Anchored.Reparented . Flip . Flip1
  liftRelativity err =
    anchored
      (const . Left . err $ Abs)
      (const . Left . err $ Rel False)
      (pure . Compose . dmap unflip1 . getCompose . getCompose)

type Typey :: Type -> Kind.Constraint
class Typey typ where
  -- | Returns the filename of the path (or `Nothing` if a directory).
  mayFilename :: Path rel typ rep -> Maybe rep

  extractType :: proxy typ -> Type
  disambiguate :: Ambiguous.Path rel rep -> Path rel typ rep
  forgetType :: Path rel typ rep -> Unambiguous.Path rel rep

  -- | Lift the `Type` to the type level. If it doesn’t have the specific
  --   `Type`, it fails with a user-provided error parameterized over the
  --   encountered `Type`.
  liftType :: (Type -> e) -> Any.Path rep -> Either e (Anchored.Path typ rep)

instance Typey 'Dir where
  mayFilename Path {} = Nothing
  disambiguate Ambiguous.Path {parents, directories, component} =
    Path {parents, directories = embed $ Both component directories, filename = Const ()}
  extractType _ = Dir
  forgetType = Compose . Unambiguous.Directory . Flip
  liftType err =
    fmap Compose
      . dtraverse
        ( unambiguous
            (pure . Flip . Flip1 . unflip . unflip1)
            (const . Left $ err File)
            . getCompose
            . getCompose
        )

instance Typey 'File where
  mayFilename Path {filename} = pure $ runIdentity filename
  disambiguate Ambiguous.Path {parents, directories, component} =
    Path {parents, directories, filename = Identity component}
  extractType _ = File
  forgetType = Compose . Unambiguous.File . Flip
  liftType err =
    fmap Compose
      . dtraverse
        ( unambiguous
            (const . Left $ err Dir)
            (pure . Flip . Flip1 . unflip . unflip1)
            . getCompose
            . getCompose
        )

type Pathy :: Relativity -> Type -> Kind.Constraint
class (Relative rel, Typey typ) => Pathy rel typ where
  lift ::
    (Relativity -> Type -> e) -> Any.Path rep -> Either e (Path rel typ rep)

instance Pathy 'Abs 'Dir where
  lift ::
    forall e rep.
    (Relativity -> Type -> e) ->
    Any.Path rep ->
    Either e (Path 'Abs 'Dir rep)
  lift err =
    anchored
      ( unambiguous (pure . unflip . unflip1) (const . Left $ err Abs File)
          . getCompose
          . getCompose
      )
      (errRel False)
      (errRel True)
    where
      errRel ::
        Bool ->
        Compose (Compose Unambiguous (Flip1 Flip rep)) Path ('Rel b) ->
        Either e (Path 'Abs 'Dir rep)
      errRel b =
        Left
          . err (Rel b)
          . unambiguous (const Dir) (const File)
          . getCompose
          . getCompose

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

type RelOps :: Relativity -> Kind.Constraint
class RelOps rel where
  ascendRelative :: Path rel 'Dir rep -> Path ('Rel 'True) 'Dir rep
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
  -- >>> serialize @Text Format.posix from
  -- "/a/d/e/"
  -- >>> let to = fromJust $ [posix|/a/b/c/|] </?> [posix|../f/g/|]
  -- >>> serialize @Text Format.posix to
  -- "/a/b/f/g/"
  -- >>> serialize @Text Format.posix $ minimalRoute from to
  -- "../../b/f/g/"
  --
  --   You can see that the directory “b” is in the result, but isn’t present in
  --   the original relative paths, so there is no way to produce the desired
  --   result without adding more context to the arguments.
  maybeRoute ::
    Path ('Rel 'True) 'Dir rep ->
    Path rel typ rep ->
    Maybe (Path ('Rel 'True) typ rep)

  -- | Like `maybeRoute`, but shortens the path as much as possible. This can
  --   only makes a difference when both paths have the same amount of
  --   reparenting. E.g.
  --
  -- >>> serialize Format.posix <$> maybeMinimalRoute @('Rel 'True) @Text [posix|../d/e/|] [posix|../d/f/|]
  -- Just "../f/"
  --
  --   whereas
  --
  -- >>> serialize Format.posix <$> maybeRoute @('Rel 'True) @Text [posix|../d/e/|] [posix|../d/f/|]
  -- Just "../../d/f/"
  --
  --   The thing to consider is that various operations can fail depending on how
  --   much reparenting is in a path. In some cases (the second argument to
  --   `maybeRoute`) more is safer, while in others (concatenating onto an
  --   absolute directory) less is safer. Carefully choosing and ordering path
  --   operations to minimize the partiality is worthwhile.
  maybeMinimalRoute ::
    (Eq rep) =>
    Path ('Rel 'True) 'Dir rep ->
    Path rel typ rep ->
    Maybe (Path ('Rel 'True) typ rep)

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
  route ::
    Path from 'Dir rep -> Path to typ rep -> Path ('Rel 'True) typ rep

  -- | Creates a path relative to the first argument that that points to the
  --   same location as the second argument.
  minimalRoute ::
    (Eq rep) =>
    Path from 'Dir rep ->
    Path to typ rep ->
    Path ('Rel 'True) typ rep

-- |
--
-- prop> maybe True (b ==) $ a </?> route  @_ @_ @Text @'File a b
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
type Prefixed :: Relativity -> Relativity -> Kind.Constraint
class Prefixed rel rel' | rel -> rel' where
  -- | Returns `False` exactly when `routePrefix` would return `Nothing`.
  --
  -- -- prop> isPrefix a b == not (isNothing $ routePrefix a b)
  isPrefix :: (Eq rep) => Path rel 'Dir rep -> Path rel typ rep -> Bool
  isPrefix a = isJust . routePrefix a

  -- | If the first argument is a prefix of the second argument, it will remove
  --   the prefix, returning a relative path. If it’s not a prefix, it returns
  --  `Nothing`.
  routePrefix ::
    (Eq rep) =>
    Path rel 'Dir rep ->
    Path rel typ rep ->
    Maybe (Path rel' typ rep)

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

type Substible :: Kind.Type -> Kind.Constraint
class Substible a where
  replace :: a -> a -> a -> a

instance Substible String where
  replace = List.replace

instance Substible Text where
  replace = Text.replace

escape' :: (Substible a) => MapF a a (a -> a) -> a -> a
escape' = \case
  TipF -> id
  BinF _ direct escaped fn fn' -> fn' . replace direct escaped . fn

serialize ::
  forall rep rel typ.
  (Relative rel, Typey typ, IsString rep, Semigroup rep, Substible rep) =>
  Format rep ->
  Path rel typ rep ->
  rep
serialize format path =
  let escapeComponent = cata escape' $ substitutions format
      prefix =
        maybe
          (root format)
          ( fromMaybe ""
              . cata (fmap $ maybe (parent format) (parent format <>))
          )
          $ mayParents path
      directory =
        cata
          ( \case
              Neither -> ""
              Both directoryName pathStr ->
                pathStr <> escapeComponent directoryName <> separator format
          )
          $ directories path
      file = maybe "" escapeComponent $ mayFilename path
   in prefix <> directory <> file

-- __TODO__: This forms a `Prism'` with `weaken`.
strengthen :: Path ('Rel 'True) typ rep -> Maybe (Path ('Rel 'False) typ rep)
strengthen path =
  if parents path == 0 then pure path {parents = Proxy} else Nothing

-- | Forget the specific type of the path (which can be recovered with
--  `anchor`).
unanchor :: (Pathy rel typ) => Path rel typ rep -> Any.Path rep
unanchor =
  dmap (Compose . Compose . dmap Flip1 . getCompose . forgetType . unflip1 . unflip)
    . getCompose
    . forgetRelativity

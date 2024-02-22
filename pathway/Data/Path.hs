{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | A representation-agnostic, structured, type-safe path library.
--
--  __NB__: This library stores paths in a normalized form. However, because it
--          is a pure library, it is not canonicalized. This makes the behavior
--          consistent across systems, whereas with canonical paths, `/a/b/../c`
--          could behave differently between Posix and Windows. If `/a/b/` is a
--          symlink to /d/e/`, then Posix would canonicalize the path to `/d/c`,
--          whereas Windows would canonicalize to `/a/c`. If you want
--          system-specific canonicalization, look at the @pathway-system@
--          package, which depends on this one.
module Data.Path
  ( Anchored (..),
    AnyPath,
    Path,
    Pathish (..),
    Pathy,
    Prefixed (..),
    RelOps (..),
    Relative,
    Relativity (..),
    Type (..),
    Typey,
    anchor,
    current,
    forgetRelativity,
    forgetType,
    minimalRoute,
    reparent,
    route,
    strengthen,
    toText,
    unanchor,
    (</>),
  )
where

import "base" Control.Applicative (Applicative (pure))
import "base" Control.Category (Category (id, (.)))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Eq (Eq ((==)))
import "base" Data.Function (($))
import "base" Data.Functor (Functor (fmap))
import "base" Data.Functor.Const (Const (Const))
import "base" Data.Functor.Identity (Identity (runIdentity))
import qualified "base" Data.Maybe as Lazy
import "base" Data.Proxy (Proxy (Proxy))
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" Data.String (IsString, String)
import "base" GHC.Natural (minusNaturalMaybe)
import "base" Numeric.Natural (Natural)
import qualified "extra" Data.List.Extra as List
import "pathway-internal" Data.Path.Internal
  ( Filename,
    List,
    Parents,
    Path (Path, directories, filename, parents),
    Relativity (Abs, Any, Rel),
    Type (Dir, File, Pathic),
    current,
    (</>),
  )
import "text" Data.Text (Text)
import qualified "text" Data.Text as Text
import "yaya" Yaya.Applied (append, length, reverse, tail)
import "yaya" Yaya.Fold
  ( Projectable (project),
    Recursive (cata),
    Steppable (embed),
    distTuple,
    gcata,
  )
import "yaya" Yaya.Fold.Native ()
import "yaya" Yaya.Pattern
  ( Maybe (Nothing),
    Pair ((:!:)),
    XNor (Both, Neither),
    fromMaybe,
    fst,
    isJust,
    maybe,
    xnor,
  )
import "yaya-containers" Yaya.Containers.Pattern.Map (MapF (BinF, TipF))
import "this" Data.Path.Format (Format (parent, root, separator, substitutions))
import "base" Prelude (Num ((+)))

-- $setup
-- >>> :seti -XDataKinds
-- >>> :seti -XQuasiQuotes
-- >>> :seti -XTypeApplications
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

-- | This is a path where we don’t know (at the type level) whether it’s
--   anchored, or whether it’s a file. `parents` is `None` for an absolute path
--   and otherwise contains the number of leading `../` parents that should be
--   produced.
--
--   There are not many operations on this type, but it is the type used for
--   generic operations like parsing and printing, with separate functions (like
--  `anchor`) to lift the contained information to the type level.
type AnyPath = Path 'Any 'Pathic

-- -- | Convert an arbitrary value representing a path to `AnyPath`.
-- --
-- --  __NB__: Don’t make an instance for `FilePath`, as it would be partial. Not
-- --          all strings represent paths. Instead, use the parsers that exist for
-- --          that purpose.
-- class Pathy path rep where
--   generalizePath :: path -> AnyPath rep

class Relative rel where
  generalizeRelativity :: Parents rel -> Parents 'Any

instance Relative 'Abs where
  generalizeRelativity () = Nothing

instance Relative 'Any where
  generalizeRelativity = id

instance Relative ('Rel 'False) where
  generalizeRelativity Proxy = pure 0

instance Relative ('Rel 'True) where
  generalizeRelativity = pure

class Typey typ where
  generalizeType :: Filename typ rep -> Filename 'Pathic rep

instance Typey 'Dir where
  generalizeType (Const ()) = Nothing

instance Typey 'File where
  generalizeType = pure . runIdentity

instance Typey 'Pathic where
  generalizeType = id

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
  maybeRoute ::
    Path ('Rel 'True) 'Dir rep ->
    Path rel typ rep ->
    Maybe (Path ('Rel 'True) typ rep)

  -- | Like `maybeRoute`, but shortens the path as much as possible. This can
  --   only makes a difference when both paths have the same amount of
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

anyToText :: (IsString a, Semigroup a, Substible a) => Format a -> AnyPath a -> a
anyToText format path =
  let escapeComponent = cata escape' $ substitutions format
      prefix =
        maybe
          (root format)
          ( fromMaybe ""
              . cata (fmap $ maybe (parent format) (parent format <>))
          )
          $ parents path
      directory =
        cata
          ( \case
              Neither -> ""
              Both directoryName pathStr ->
                pathStr <> escapeComponent directoryName <> separator format
          )
          $ directories path
      file = maybe "" escapeComponent $ filename path
   in prefix <> directory <> file

toText ::
  (Pathy rel typ, IsString a, Semigroup a, Substible a) => Format a -> Path rel typ a -> a
toText format = anyToText format . unanchor

-- __TODO__: This forms a `Prism'` with `weaken`.
strengthen :: Path ('Rel 'True) typ rep -> Maybe (Path ('Rel 'False) typ rep)
strengthen path =
  if parents path == 0 then pure path {parents = Proxy} else Nothing

data Anchored rep
  = AbsDir (Path 'Abs 'Dir rep)
  | AbsFile (Path 'Abs 'File rep)
  | RelDir (Path ('Rel 'False) 'Dir rep)
  | RelFile (Path ('Rel 'False) 'File rep)
  | ReparentedDir (Path ('Rel 'True) 'Dir rep)
  | ReparentedFile (Path ('Rel 'True) 'File rep)

-- | Discover the specific type of a `Path`.
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

forgetRelativity :: (Relative rel) => Path rel typ rep -> Path 'Any typ rep
forgetRelativity path = path {parents = generalizeRelativity $ parents path}

forgetType :: (Typey typ) => Path rel typ rep -> Path rel 'Pathic rep
forgetType path = path {filename = generalizeType $ filename path}

-- | Forget the specific type of the path (which can be recovered with
--  `anchor`).
unanchor :: (Pathy rel typ) => Path rel typ rep -> AnyPath rep
unanchor = forgetRelativity . forgetType

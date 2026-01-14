{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- | Various parsers for paths.
--
--  __NB__: This doesn’t provide parsers for `Path`, only `AnyPath`. If you are
--          parsing at compile time, "Data.Path.TH" has
--         `Language.Haskell.TH.Quote.QuasiQuoter`s that will will parse
--          directly to the specific `Path` type that is needed. For runtime
--          parsing, you are better off using @`anchor` `.` `parse` (`path`
--         `local`) ""@ and then creating informative failures for the invalid
--          cases.
module Data.Path.Parser
  ( ambiguousPath,
    directory,
    path,
  )
where

import "base" Control.Applicative (pure, (<*), (<*>), (<|>))
import "base" Control.Category ((.))
import "base" Control.Monad.Fail (fail)
import "base" Data.Bool ((&&))
import "base" Data.Char (Char, isPrint)
import "base" Data.Either (Either (Left))
import "base" Data.Eq ((/=))
import "base" Data.Foldable (fold, foldr)
import "base" Data.Function (flip, ($))
import "base" Data.Functor (fmap, void, (<$), (<$>))
import "base" Data.Functor.Compose (Compose (Compose))
import "base" Data.Functor.Const (Const (Const))
import "base" Data.List (length)
import "base" Data.Monoid (Monoid)
import "base" Data.Ord (Ord)
import "base" Data.Proxy (Proxy (Proxy))
import "base" Numeric.Natural (Natural)
import "megaparsec" Text.Megaparsec qualified as MP
import "pathway-internal" Data.Path.Internal
  ( List (List),
    Path (Path),
    directories,
    filename,
    parents,
  )
import "yaya" Yaya.Applied (reverse')
import "yaya" Yaya.Fold (Mu, Projectable, Steppable, cata, embed)
import "yaya" Yaya.Pattern (Maybe (Just, Nothing), XNor (Both, Neither))
import "yaya-containers" Yaya.Containers.Pattern.Map (MapF (BinF, TipF))
import "yaya-unsafe" Yaya.Unsafe.Fold (unsafeCata)
import "this" Data.Path (Type (Dir))
import "this" Data.Path.Ambiguous qualified as Ambiguous
import "this" Data.Path.Anchored qualified as Anchored
import "this" Data.Path.Any qualified as Any
import "this" Data.Path.Format
  ( Format,
    current,
    parent,
    root,
    separator,
    substitutions,
  )
import "this" Data.Path.Functor (Flip (Flip), Flip1 (Flip1))
import "this" Data.Path.Unambiguous qualified as Unambiguous
import "base" Prelude (fromIntegral)
#if MIN_VERSION_base(4, 17, 0)
import "base" Data.Type.Equality (type (~))
#endif

-- $setup
-- >>> import qualified "megaparsec" Text.Megaparsec as MP
-- >>> import "this" Data.Path.Format (posix)

unsafeReverse ::
  (Projectable (->) t (XNor a), Steppable (->) u (XNor a)) => t -> u
unsafeReverse = embed . flip (unsafeCata reverse') Neither

-- |
-- >>> MP.parse (rootDirectory posix) "" "/"
-- Right ()
-- >>> MP.parse (path posix) "" "/"
-- Right (Absolute (Compose (Compose (Directory (Flip1 {unflip1 = Flip {unflip = Path {parents = (), directories = List (embed Neither), filename = Const ()}}})))))
rootDirectory ::
  (MP.Stream s, Ord e) => Format (MP.Tokens s) -> MP.Parsec e s ()
rootDirectory = void . MP.chunk . root

-- |
-- >>> MP.parse (currentDirectory posix) "" "./"
-- Right ()
-- >>> MP.parse (path posix) "" "./"
-- Right (Relative (Compose (Compose (Directory (Flip1 {unflip1 = Flip {unflip = Path {parents = Proxy, directories = List (embed Neither), filename = Const ()}}})))))
currentDirectory ::
  (MP.Stream s, Ord e) => Format (MP.Tokens s) -> MP.Parsec e s ()
currentDirectory = void . MP.chunk . current

-- |
-- >>> MP.parse (parents' posix) "" "../"
-- Right 1
-- >>> MP.parse (path posix) "" "../"
-- Right (Reparented (Compose (Compose (Directory (Flip1 {unflip1 = Flip {unflip = Path {parents = 1, directories = List (embed Neither), filename = Const ()}}})))))
parents' :: (MP.Stream s, Ord e) => Format (MP.Tokens s) -> MP.Parsec e s Natural
parents' = fmap (fromIntegral . length) . MP.many . MP.chunk . parent

anchor ::
  (MP.Stream s, Ord e) => Format (MP.Tokens s) -> MP.Parsec e s (Maybe Natural)
anchor format =
  Nothing <$ rootDirectory format
    <|> pure 0 <$ currentDirectory format
    <|> pure <$> parents' format

escapeChar ::
  (MP.Stream s, Ord e) =>
  MapF (MP.Tokens s) (MP.Tokens s) (MP.Parsec e s (MP.Tokens s)) ->
  MP.Parsec e s (MP.Tokens s)
escapeChar = \case
  TipF -> fail "no parser"
  BinF _ direct escaped fn fn' -> fn' <|> direct <$ MP.chunk escaped <|> fn

standardChar ::
  forall s e.
  (MP.Stream s, MP.Token s ~ Char, Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (MP.Token s)
standardChar format =
  MP.satisfy
    ( \c ->
        MP.tokenToChunk (Proxy :: Proxy s) c /= separator format && isPrint c
    )
    MP.<?> "standard character"

componentChar ::
  forall s e.
  (MP.Stream s, MP.Token s ~ Char, Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (MP.Tokens s)
componentChar format =
  cata escapeChar (substitutions format)
    <|> MP.tokenToChunk (Proxy :: Proxy s) <$> standardChar format

-- |
--
-- >>> MP.parse (component' posix) "" "env"
-- Right "env"
--
-- >>> MP.parse (path posix) "" "env"
-- Right (Relative (Compose (Compose (File (Flip1 {unflip1 = Flip {unflip = Path {parents = Proxy, directories = List (embed Neither), filename = Identity "env"}}})))))
component' ::
  (MP.Stream s, MP.Token s ~ Char, Monoid (MP.Tokens s), Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (MP.Tokens s)
component' = fmap fold . MP.some . componentChar

-- |
--
-- >>> MP.parse (directoryName posix) "" "bin/"
-- Right "bin"
--
-- >>> MP.parse (path posix) "" "bin/"
-- Right (Relative (Compose (Compose (Directory (Flip1 {unflip1 = Flip {unflip = Path {parents = Proxy, directories = List (embed (Both "bin" (embed Neither))), filename = Const ()}}})))))
directoryName ::
  (MP.Stream s, MP.Token s ~ Char, Monoid (MP.Tokens s), Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (MP.Tokens s)
directoryName format = component' format <* MP.chunk (separator format)

-- |
--
-- >>> MP.parse (directories' posix) "" "bin/bar/baz/"
-- Right (embed (Both "baz" (embed (Both "bar" (embed (Both "bin" (embed Neither)))))))
--
-- >>> MP.parse (path posix) "" "bin/bar/baz/"
-- Right (Relative (Compose (Compose (Directory (Flip1 {unflip1 = Flip {unflip = Path {parents = Proxy, directories = List (embed (Both "baz" (embed (Both "bar" (embed (Both "bin" (embed Neither))))))), filename = Const ()}}})))))
--
--  __TODO__: Support interior and final @.@ & @..@ (final ones always indicate
--            a directory, even without a trailing slash – also, how portable is
--            this?).
directories' ::
  (MP.Stream s, MP.Token s ~ Char, Monoid (MP.Tokens s), Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (Mu (XNor (MP.Tokens s)))
directories' = fmap unsafeReverse . MP.many . MP.try . directoryName

protoPath ::
  (MP.Stream s, MP.Token s ~ Char, Monoid (MP.Tokens s), Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (Maybe Natural, Mu (XNor (MP.Tokens s)), Maybe (MP.Tokens s))
protoPath format =
  (,,)
    <$> anchor format
    <*> directories' format
    <*> MP.option Nothing (pure <$> component' format)

-- | This will parse a path without a trailing separator as a file and one with
--   a trailing separator as a directory. See `directory` for when you know you
--   have a directory and need more lax parsing.
--
-- >>> MP.parse (path posix) "" "../../d/e/"
-- Right (Reparented (Compose (Compose (Directory (Flip1 {unflip1 = Flip {unflip = Path {parents = 2, directories = List (embed (Both "e" (embed (Both "d" (embed Neither))))), filename = Const ()}}})))))
-- >>> MP.parse (path posix) "" "../../../b/f/g/"
-- Right (Reparented (Compose (Compose (Directory (Flip1 {unflip1 = Flip {unflip = Path {parents = 3, directories = List (embed (Both "g" (embed (Both "f" (embed (Both "b" (embed Neither))))))), filename = Const ()}}})))))
path ::
  (MP.Stream s, MP.Token s ~ Char, Monoid (MP.Tokens s), Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (Any.Path (MP.Tokens s))
path =
  fmap
    ( ( \case
          (Nothing, directories, Nothing) ->
            Anchored.Absolute $ dir Path {parents = (), directories, filename = Const ()}
          (Nothing, directories, Just filename) ->
            Anchored.Absolute $ file Path {parents = (), directories, filename = pure filename}
          (Just 0, directories, Nothing) ->
            Anchored.Relative $ dir Path {parents = Proxy, directories, filename = Const ()}
          (Just 0, directories, Just filename) ->
            Anchored.Relative $ file Path {parents = Proxy, directories, filename = pure filename}
          (Just parents, directories, Nothing) ->
            Anchored.Reparented $ dir Path {parents, directories, filename = Const ()}
          (Just parents, directories, Just filename) ->
            Anchored.Reparented $ file Path {parents, directories, filename = pure filename}
      )
        . \(p, d, f) -> (p, List d, f)
    )
    . protoPath
  where
    dir = Compose . Compose . Unambiguous.Directory . Flip1 . Flip
    file = Compose . Compose . Unambiguous.File . Flip1 . Flip

-- | Knowing that we’re parsing a directory, this can be a bit more lax in
--   parsing. E.g., this will parse as a directory even without a trailing
--   separator on the final component.
directory ::
  (MP.Stream s, MP.Token s ~ Char, Monoid (MP.Tokens s), Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (Anchored.Path 'Dir (MP.Tokens s))
directory =
  fmap
    ( Compose . \case
        (Nothing, dir, com) ->
          Anchored.Absolute . Flip $
            Flip1
              Path
                { parents = (),
                  directories = List $ foldr ((embed .) . Both) dir com,
                  filename = Const ()
                }
        (Just 0, dir, com) ->
          Anchored.Relative . Flip $
            Flip1
              Path
                { parents = Proxy,
                  directories = List $ foldr ((embed .) . Both) dir com,
                  filename = Const ()
                }
        (Just parents, dir, com) ->
          Anchored.Reparented . Flip $
            Flip1
              Path
                { parents,
                  directories = List $ foldr ((embed .) . Both) dir com,
                  filename = Const ()
                }
    )
    . protoPath

-- | When we need lax parsing, but don’t know at the time of parsing whether we
--   have a file or directory, this parses to the more flexible
--  `Anchored.AmbiguousPath` that can be disambiguated later (if at all).
ambiguousPath ::
  (MP.Stream s, MP.Token s ~ Char, Monoid (MP.Tokens s), Ord e) =>
  Format (MP.Tokens s) ->
  -- | Returns in `Either` because some paths (that is, anything with a trailing
  --   directory delimiter or anything that has no components like @/@ or @..@)
  --   are unambiguously directories.
  MP.Parsec e s (Either (Anchored.Path 'Dir (MP.Tokens s)) (Anchored.AmbiguousPath (MP.Tokens s)))
ambiguousPath =
  fmap
    ( ( \case
          (Nothing, directories, Nothing) ->
            Left . Compose . Anchored.Absolute $ dir Path {parents = (), directories, filename = Const ()}
          (Nothing, directories, Just component) ->
            pure . Compose . Anchored.Absolute $ Flip Ambiguous.Path {parents = (), directories, component}
          (Just 0, directories, Nothing) ->
            Left . Compose . Anchored.Relative $ dir Path {parents = Proxy, directories, filename = Const ()}
          (Just 0, directories, Just component) ->
            pure . Compose . Anchored.Relative $ Flip Ambiguous.Path {parents = Proxy, directories, component}
          (Just parents, directories, Nothing) ->
            Left . Compose . Anchored.Reparented $ dir Path {parents, directories, filename = Const ()}
          (Just parents, directories, Just component) ->
            pure . Compose . Anchored.Reparented $ Flip Ambiguous.Path {parents, directories, component}
      )
        . \(p, d, f) -> (p, List d, f)
    )
    . protoPath
  where
    dir = Flip . Flip1

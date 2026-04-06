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
  ( -- ambiguousPath,
    directory,
    path,
  )
where

import "base" Control.Applicative (pure, (<*), (<*>), (<|>))
import "base" Control.Category ((.))
import "base" Data.Eq ((/=))
import "base" Data.Foldable (fold, foldr)
import "base" Data.Function (flip, ($))
import "base" Data.Functor (fmap, void, (<$), (<$>))
import "base" Data.Functor.Const (Const (Const))
import "base" Data.List (length)
import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.Monoid (Monoid)
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
import "yaya" Yaya.Pattern (Maybe (Nothing), XNor (Both, Neither))
import "yaya-containers" Yaya.Containers.Pattern.Map (MapF (BinF, TipF))
import "yaya-unsafe" Yaya.Unsafe.Fold (unsafeCata)
import "this" Data.Path (AnyPath)
import "this" Data.Path.Format
  ( Format,
    current,
    parent,
    root,
    separator,
    substitutions,
  )
import "this" Data.Path.Relativity (Relativity (Any))
import "this" Data.Path.Type (Type (Dir))
import "base" Prelude (fromIntegral)

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> import Data.Path.Format (posix)
-- >>> import qualified "megaparsec" Text.Megaparsec as MP

unsafeReverse ::
  (Projectable (->) t (XNor a), Steppable (->) u (XNor a)) => t -> u
unsafeReverse = embed . flip (unsafeCata reverse') Neither

-- |
-- >>> MP.parse (rootDirectory posix) "" "/"
-- Right ()
-- >>> MP.parse (path posix) "" "/"
-- Right (Path {parents = Nothing, directories = List (embed Neither), filename = Nothing})
rootDirectory :: (MP.MonadParsec v s p) => Format (MP.Tokens s) -> p ()
rootDirectory = void . MP.chunk . root

-- |
-- >>> MP.parse (currentDirectory posix) "" "./"
-- Right ()
-- >>> MP.parse (path posix) "" "./"
-- Right (Path {parents = Just 0, directories = List (embed Neither), filename = Nothing})
currentDirectory :: (MP.MonadParsec v s p) => Format (MP.Tokens s) -> p ()
currentDirectory = void . MP.chunk . current

-- |
-- >>> MP.parse (parents' posix) "" "../"
-- Right 1
-- >>> MP.parse (path posix) "" "../"
-- Right (Path {parents = Just 1, directories = List (embed Neither), filename = Nothing})
parents' :: (MP.MonadParsec v s p) => Format (MP.Tokens s) -> p Natural
parents' = fmap (fromIntegral . length) . MP.many . MP.chunk . parent

anchor :: (MP.MonadParsec v s p) => Format (MP.Tokens s) -> p (Maybe Natural)
anchor format =
  Nothing <$ rootDirectory format
    <|> pure 0 <$ currentDirectory format
    <|> pure <$> parents' format

escapeChar ::
  (MP.MonadParsec v s p) =>
  MapF (MP.Tokens s) (MP.Tokens s) (p (MP.Tokens s)) -> p (MP.Tokens s)
escapeChar = \case
  TipF -> MP.unexpected $ MP.Label ('e' :| "scape character")
  BinF _ direct escaped fn fn' -> fn' <|> direct <$ MP.chunk escaped <|> fn

standardChar ::
  forall v s p. (MP.MonadParsec v s p) => Format (MP.Tokens s) -> p (MP.Token s)
standardChar format =
  MP.satisfy
    ((separator format /=) . MP.tokenToChunk (Proxy :: Proxy s))
    MP.<?> "standard character"

componentChar ::
  forall v s p. (MP.MonadParsec v s p) => Format (MP.Tokens s) -> p (MP.Tokens s)
componentChar format =
  cata escapeChar (substitutions format)
    <|> MP.tokenToChunk (Proxy :: Proxy s) <$> standardChar format

-- |
--
-- >>> MP.parse (component posix) "" "env"
-- Right "env"
--
-- >>> MP.parse (path posix) "" "env"
-- Right (Path {parents = Just 0, directories = List (embed Neither), filename = Just "env"})
component ::
  (MP.MonadParsec v s p, Monoid (MP.Tokens s)) =>
  Format (MP.Tokens s) -> p (MP.Tokens s)
component = fmap fold . MP.some . componentChar

-- |
--
-- >>> MP.parse (directoryName posix) "" "bin/"
-- Right "bin"
--
-- >>> MP.parse (path posix) "" "bin/"
-- Right (Path {parents = Just 0, directories = List (embed (Both "bin" (embed Neither))), filename = Nothing})
directoryName ::
  (MP.MonadParsec v s p, Monoid (MP.Tokens s)) =>
  Format (MP.Tokens s) -> p (MP.Tokens s)
directoryName format = component format <* MP.chunk (separator format)

-- |
--
-- >>> MP.parse (directories' posix) "" "bin/bar/baz/"
-- Right (embed (Both "baz" (embed (Both "bar" (embed (Both "bin" (embed Neither)))))))
--
-- >>> MP.parse (path posix) "" "bin/bar/baz/"
-- Right (Path {parents = Just 0, directories = List (embed (Both "baz" (embed (Both "bar" (embed (Both "bin" (embed Neither))))))), filename = Nothing})
--
--  __TODO__: Support interior and final @.@ & @..@ (final ones always indicate
--            a directory, even without a trailing slash – also, how portable is
--            this?).
directories' ::
  (MP.MonadParsec v s p, Monoid (MP.Tokens s)) =>
  Format (MP.Tokens s) -> p (Mu (XNor (MP.Tokens s)))
directories' = fmap unsafeReverse . MP.many . MP.try . directoryName

protoPath ::
  (MP.MonadParsec v s p, Monoid (MP.Tokens s)) =>
  Format (MP.Tokens s) ->
  p (Maybe Natural, Mu (XNor (MP.Tokens s)), Maybe (MP.Tokens s))
protoPath format =
  (,,)
    <$> anchor format
    <*> directories' format
    <*> MP.option Nothing (pure <$> component format)

-- | This will parse a path without a trailing separator as a file and one with
--   a trailing separator as a directory. See `directory` for when you know you
--   have a directory and need more lax parsing.
--
-- >>> MP.parse (path posix) "" "../../d/e/"
-- Right (Path {parents = Just 2, directories = List (embed (Both "e" (embed (Both "d" (embed Neither))))), filename = Nothing})
-- >>> MP.parse (path posix) "" "../../../b/f/g/"
-- Right (Path {parents = Just 3, directories = List (embed (Both "g" (embed (Both "f" (embed (Both "b" (embed Neither))))))), filename = Nothing})
path ::
  (MP.MonadParsec v s p, Monoid (MP.Tokens s)) =>
  Format (MP.Tokens s) -> p (AnyPath (MP.Tokens s))
path =
  fmap
    ( \(parents, dir, filename) ->
        Path {parents, directories = List dir, filename}
    )
    . protoPath

-- | Knowing that we’re parsing a directory, this can be a bit more lax in
--   parsing. E.g., this will parse as a directory even without a trailing
--   separator on the final component.
directory ::
  (MP.MonadParsec v s p, Monoid (MP.Tokens s)) =>
  Format (MP.Tokens s) -> p (Path 'Any 'Dir (MP.Tokens s))
directory =
  fmap
    ( \(parents, dir, filenm) ->
        Path
          { parents,
            directories = List $ foldr ((embed .) . Both) dir filenm,
            filename = Const ()
          }
    )
    . protoPath

-- -- | When we need lax parsing, but don’t know at the time of parsing whether we
-- --   have a file or directory, this parses to the more flexible
-- --  `AnyAmbiguousPath` that can be disambiguated later (if at all).
-- ambiguousPath :: (MP.MonadParsec v FilePath p) => Format -> p AnyAmbiguousPath

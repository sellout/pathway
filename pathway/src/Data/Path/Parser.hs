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
  ( -- ambiguousPath,
    directory,
    path,
  )
where

import "base" Control.Applicative (pure, (<*), (<*>), (<|>))
import "base" Control.Category ((.))
import "base" Control.Monad.Fail (fail)
import "base" Data.Bool ((&&))
import "base" Data.Char (Char, isPrint)
import "base" Data.Eq ((/=))
import "base" Data.Foldable (fold, foldr)
import "base" Data.Function (flip, ($))
import "base" Data.Functor (fmap, void, (<$), (<$>))
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
import "yaya" Yaya.Pattern (Maybe (Nothing), XNor (Both, Neither))
import "yaya-containers" Yaya.Containers.Pattern.Map (MapF (BinF, TipF))
import "yaya-unsafe" Yaya.Unsafe.Fold (unsafeCata)
import "this" Data.Path (AnyPath, Relativity (Any), Type (Dir))
import "this" Data.Path.Format
  ( Format,
    current,
    parent,
    root,
    separator,
    substitutions,
  )
import "base" Prelude (fromIntegral)
#if MIN_VERSION_base(4, 17, 0)
import "base" Data.Type.Equality (type (~))
#endif

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
rootDirectory ::
  (MP.Stream s, Ord e) => Format (MP.Tokens s) -> MP.Parsec e s ()
rootDirectory = void . MP.chunk . root

-- |
-- >>> MP.parse (currentDirectory posix) "" "./"
-- Right ()
-- >>> MP.parse (path posix) "" "./"
-- Right (Path {parents = Just 0, directories = List (embed Neither), filename = Nothing})
currentDirectory ::
  (MP.Stream s, Ord e) => Format (MP.Tokens s) -> MP.Parsec e s ()
currentDirectory = void . MP.chunk . current

-- |
-- >>> MP.parse (parents' posix) "" "../"
-- Right 1
-- >>> MP.parse (path posix) "" "../"
-- Right (Path {parents = Just 1, directories = List (embed Neither), filename = Nothing})
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
-- >>> MP.parse (component posix) "" "env"
-- Right "env"
--
-- >>> MP.parse (path posix) "" "env"
-- Right (Path {parents = Just 0, directories = List (embed Neither), filename = Just "env"})
component ::
  (MP.Stream s, MP.Token s ~ Char, Monoid (MP.Tokens s), Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (MP.Tokens s)
component = fmap fold . MP.some . componentChar

-- |
--
-- >>> MP.parse (directoryName posix) "" "bin/"
-- Right "bin"
--
-- >>> MP.parse (path posix) "" "bin/"
-- Right (Path {parents = Just 0, directories = List (embed (Both "bin" (embed Neither))), filename = Nothing})
directoryName ::
  (MP.Stream s, MP.Token s ~ Char, Monoid (MP.Tokens s), Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (MP.Tokens s)
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
  (MP.Stream s, MP.Token s ~ Char, Monoid (MP.Tokens s), Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (AnyPath (MP.Tokens s))
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
  (MP.Stream s, MP.Token s ~ Char, Monoid (MP.Tokens s), Ord e) =>
  Format (MP.Tokens s) ->
  MP.Parsec e s (Path 'Any 'Dir (MP.Tokens s))
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
-- ambiguousPath :: Format -> MP.Parsec e FilePath AnyAmbiguousPath

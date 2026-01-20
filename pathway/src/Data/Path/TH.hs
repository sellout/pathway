{-# LANGUAGE Safe #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Path.TH
  ( path,
    posix,
    windows,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category (id, (.))
import "base" Control.Monad.Fail (fail)
import "base" Data.Either (either)
import "base" Data.Function (const, ($))
import "base" Data.Functor.Const (Const (Const))
import "base" Data.Functor.Identity (Identity (Identity))
import "base" Data.Proxy (Proxy (Proxy))
import "base" Data.String (String, fromString)
import "base" Data.Void (Void)
import "base" Numeric.Natural (Natural)
import "base" Text.Show (show)
import "megaparsec" Text.Megaparsec qualified as MP
import "pathway-internal" Data.Path.Internal
  ( Path (Path),
    directories,
    filename,
    parents,
  )
import "template-haskell" Language.Haskell.TH.Quote qualified as TH
import "template-haskell" Language.Haskell.TH.Syntax qualified as TH
import "yaya" Yaya.Fold (Recursive, cata, embed)
import "yaya" Yaya.Pattern (Maybe (Nothing), XNor (Both, Neither), maybe, xnor)
import "this" Data.Path (AnyPath)
import "this" Data.Path.Format (Format)
import "this" Data.Path.Format qualified as Format
import "this" Data.Path.Parser qualified as Parser
import "base" Prelude (fromIntegral)

-- $setup
-- >>> :seti -XQuasiQuotes
-- >>> import Data.Path (Relativity (Abs), Type (File))

deconstructXNor :: (a -> TH.Exp) -> (b -> TH.Exp) -> XNor a b -> TH.Exp
deconstructXNor f g =
  xnor (TH.ConE 'Neither) (\a -> TH.AppE (TH.AppE (TH.ConE 'Both) (f a)) . g)

deconstructRec :: (Recursive (->) t f) => (f TH.Exp -> TH.Exp) -> t -> TH.Exp
deconstructRec φ = cata (TH.AppE (TH.VarE 'embed) . φ)

deconstructMaybe :: (a -> TH.Exp) -> Maybe a -> TH.Exp
deconstructMaybe = maybe (TH.TupE [])

deconstructMaybe' :: (a -> TH.Exp) -> Maybe a -> TH.Exp
deconstructMaybe' fn =
  maybe
    (TH.AppE (TH.ConE 'Const) $ TH.TupE [])
    (TH.AppE (TH.ConE 'Identity) . fn)

textLitE :: String -> TH.Exp
textLitE = TH.AppE (TH.VarE 'fromString) . TH.LitE . TH.StringL

deconstructNat :: Natural -> TH.Exp
deconstructNat 0 = TH.AppTypeE (TH.ConE 'Proxy) (TH.ConT ''Natural)
deconstructNat n =
  TH.SigE (TH.LitE . TH.IntegerL $ fromIntegral n) $ TH.ConT ''Natural

-- | While this deconstructs `AnyPath`, the resulting `TH.Exp` represents a
--   specific @`IsString` rep => forall rel typ. `Path` rel typ rep@ type.
deconstructAnyPath :: AnyPath String -> TH.Exp
deconstructAnyPath (Path p d f) =
  TH.RecConE
    'Path
    [ ('parents, deconstructMaybe deconstructNat p),
      ('directories, deconstructRec (deconstructXNor textLitE id) d),
      ('filename, deconstructMaybe' textLitE f)
    ]

path :: Format String -> String -> TH.Q TH.Exp
path format =
  either (fail . show) (pure . deconstructAnyPath)
    . MP.parse (Parser.path @_ @Void format) ""

pathQuoter :: Format String -> TH.QuasiQuoter
pathQuoter format =
  TH.QuasiQuoter
    { TH.quoteDec = const $ fail "Meh",
      TH.quoteExp = path format,
      TH.quotePat = const $ fail "Meh",
      TH.quoteType = const $ fail "Meh"
    }

-- |
--
-- >>> [posix|/usr/bin/env|] :: Path 'Abs 'File String
-- Path {parents = (), directories = List (embed (Both "bin" (embed (Both "usr" (embed Neither))))), filename = Identity "env"}
posix :: TH.QuasiQuoter
posix = pathQuoter Format.posix

-- |
--
-- >>> [windows|\usr\bin\env|] :: Path 'Abs 'File String
-- Path {parents = (), directories = List (embed (Both "bin" (embed (Both "usr" (embed Neither))))), filename = Identity "env"}
windows :: TH.QuasiQuoter
windows = pathQuoter $ Format.windows Nothing

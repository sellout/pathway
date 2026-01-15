{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested `Parents` and `Filename` constraints.
{-# LANGUAGE UndecidableInstances #-}
-- __NB__: Because QuickCheck doesn’t provide @`QC.Arbitrary` `Natural`@.
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Test.Path.QuickCheck
  ( arbitraryNonEmptyText,
    arbitraryPath,
    arbitraryAbsPath,
    arbitraryRelPath,
    arbitraryDir,
    arbitraryAbsDir,
    arbitraryRelDir,
  )
where

import "QuickCheck" Test.QuickCheck qualified as QC
import "base" Control.Applicative (pure, (<*>))
import "base" Control.Category ((.))
import "base" Data.Bool (Bool (False))
import "base" Data.Function (const, ($))
import "base" Data.Functor ((<$>))
import "base" Data.Functor.Const (Const (Const))
import "base" Data.Proxy (Proxy (Proxy))
import "base" Data.String (IsString, fromString)
import "base" Numeric.Natural (Natural)
import "pathway-internal" Data.Path.Internal
  ( Filename,
    List,
    Parents,
    Path (Path),
    Relativity (Abs, Rel),
    Type (Dir),
  )
import "quickcheck-instances" Test.QuickCheck.Instances.Strict ()
import "yaya" Yaya.Applied (naturals, take)
import "yaya" Yaya.Fold (Nu)
import "yaya-quickcheck" Yaya.QuickCheck.Fold (arbitrarySteppable)

arbitraryNonEmptyText :: (IsString s) => QC.Gen s
arbitraryNonEmptyText = fromString <$> QC.listOf1 QC.arbitraryPrintableChar

arbitraryDirectories :: QC.Gen s -> QC.Gen (List s)
arbitraryDirectories = arbitrarySteppable . QC.liftArbitrary2

-- | This can generate any kind of path, but some paths have trivial generators
--   for `parents` and/or `filename`, so there are specializations provided for
--   those cases.
arbitraryPath ::
  QC.Gen (Parents rel) ->
  (QC.Gen rep -> QC.Gen (Filename typ rep)) ->
  QC.Gen rep ->
  QC.Gen (Path rel typ rep)
arbitraryPath rel typ rep =
  Path <$> rel <*> arbitraryDirectories rep <*> typ rep

arbitraryAbsPath ::
  (QC.Gen rep -> QC.Gen (Filename typ rep)) ->
  QC.Gen rep ->
  QC.Gen (Path 'Abs typ rep)
arbitraryAbsPath = arbitraryPath (pure ())

arbitraryRelPath ::
  (QC.Gen rep -> QC.Gen (Filename typ rep)) ->
  QC.Gen rep ->
  QC.Gen (Path ('Rel 'False) typ rep)
arbitraryRelPath = arbitraryPath (pure Proxy)

arbitraryDir ::
  QC.Gen (Parents rel) ->
  QC.Gen rep ->
  QC.Gen (Path rel 'Dir rep)
arbitraryDir rel = arbitraryPath rel . const . pure $ Const ()

arbitraryAbsDir ::
  QC.Gen rep ->
  QC.Gen (Path 'Abs 'Dir rep)
arbitraryAbsDir = arbitraryAbsPath . const . pure $ Const ()

arbitraryRelDir ::
  QC.Gen rep ->
  QC.Gen (Path ('Rel 'False) 'Dir rep)
arbitraryRelDir = arbitraryRelPath . const . pure $ Const ()

instance QC.Arbitrary Natural where
  arbitrary = QC.arbitrarySizedNatural
  shrink x = take x (naturals :: Nu ((,) Natural))

instance
  (QC.Arbitrary (Parents rel), QC.Arbitrary1 (Filename typ)) =>
  QC.Arbitrary1 (Path rel typ)
  where
  liftArbitrary = arbitraryPath QC.arbitrary QC.liftArbitrary

-- | This requires `IsString` because we want to make sure the generated path
--   components are non-empty, and use a printable character. This is not a hard
--   requirement of Pathway, though, so it might be better to just use
--   @`Arbitrary` rep@.
instance
  (QC.Arbitrary1 (Path rel typ), IsString rep) =>
  QC.Arbitrary (Path rel typ rep)
  where
  arbitrary = QC.liftArbitrary arbitraryNonEmptyText

{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested `Parents` and `Filename` constraints.
{-# LANGUAGE UndecidableInstances #-}
-- __NB__: Because QuickCheck doesn’t provide @`QC.Arbitrary` `Natural`@.
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Path.QuickCheck
  ( arbitraryNonEmptyText,
    arbitraryPath,
  )
where

import "QuickCheck" Test.QuickCheck qualified as QC
import "base" Control.Applicative ((<*>))
import "base" Control.Category ((.))
import "base" Data.Functor ((<$>))
import "base" Data.String (IsString, fromString)
import "base" Numeric.Natural (Natural)
import "pathway-internal" Data.Path.Internal
  ( Filename,
    List,
    Parents,
    Path (Path),
  )
import "quickcheck-instances" Test.QuickCheck.Instances.Strict ()
import "yaya" Yaya.Applied (naturals, take)
import "yaya" Yaya.Fold (Nu)
import "yaya-quickcheck" Yaya.QuickCheck.Fold (arbitrarySteppable)

arbitraryNonEmptyText :: (IsString s) => QC.Gen s
arbitraryNonEmptyText = fromString <$> QC.listOf1 QC.arbitraryPrintableChar

arbitraryDirectories :: QC.Gen s -> QC.Gen (List s)
arbitraryDirectories = arbitrarySteppable . QC.liftArbitrary2

arbitraryPath ::
  QC.Gen (Parents rel) ->
  (QC.Gen rep -> QC.Gen (Filename typ rep)) ->
  QC.Gen rep ->
  QC.Gen (Path rel typ rep)
arbitraryPath rel typ rep =
  Path <$> rel <*> arbitraryDirectories rep <*> typ rep

instance QC.Arbitrary Natural where
  arbitrary = QC.arbitrarySizedNatural
  shrink x = take x (naturals :: Nu ((,) Natural))

instance
  (QC.Arbitrary (Parents rel), QC.Arbitrary1 (Filename typ)) =>
  QC.Arbitrary1 (Path rel typ)
  where
  liftArbitrary = arbitraryPath QC.arbitrary QC.liftArbitrary

instance
  (QC.Arbitrary1 (Path rel typ), IsString rep) =>
  QC.Arbitrary (Path rel typ rep)
  where
  arbitrary = QC.liftArbitrary arbitraryNonEmptyText

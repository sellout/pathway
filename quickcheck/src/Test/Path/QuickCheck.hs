{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested `Parents` and `Filename` constraints.
{-# LANGUAGE UndecidableInstances #-}
-- "Test.QuickCheck.Instances.Strict" is only inferred ‘Safe’.
{-# OPTIONS_GHC -Wno-inferred-safe-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
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
import "pathway-internal" Data.Path.Internal
  ( Filename,
    List,
    Parents,
    Path (Path),
  )
import "quickcheck-instances" Test.QuickCheck.Instances.Strict ()
import "yaya-quickcheck" Yaya.QuickCheck.Fold (arbitrarySteppable)
#if !MIN_VERSION_QuickCheck (2, 17, 0)
import "base" Numeric.Natural (Natural)
import "yaya" Yaya.Applied (naturals, take)
import "yaya" Yaya.Fold (Nu)
#endif

-- | Generates non-empty strings of printable characters, suitable for path
--   components.
--
-- @since 99999
arbitraryNonEmptyText :: (IsString s) => QC.Gen s
arbitraryNonEmptyText = fromString <$> QC.listOf1 QC.arbitraryPrintableChar

arbitraryDirectories :: QC.Gen s -> QC.Gen (List s)
arbitraryDirectories = arbitrarySteppable . QC.liftArbitrary2

-- | Generates a `Path` from generators for its leading @../@ components, its
--   final component (given a generator for components), and its components.
--
-- @since 99999
arbitraryPath ::
  QC.Gen (Parents rel) ->
  (QC.Gen rep -> QC.Gen (Filename typ rep)) ->
  QC.Gen rep ->
  QC.Gen (Path rel typ rep)
arbitraryPath rel typ rep =
  Path <$> rel <*> arbitraryDirectories rep <*> typ rep

#if !MIN_VERSION_QuickCheck (2, 17, 0)
-- | @since 99999
instance QC.Arbitrary Natural where
  arbitrary = QC.arbitrarySizedNatural
  shrink x = take x (naturals :: Nu ((,) Natural))
#endif

-- | @since 99999
instance
  (QC.Arbitrary (Parents rel), QC.Arbitrary1 (Filename typ)) =>
  QC.Arbitrary1 (Path rel typ)
  where
  liftArbitrary = arbitraryPath QC.arbitrary QC.liftArbitrary

-- | @since 99999
instance
  (QC.Arbitrary1 (Path rel typ), IsString rep) =>
  QC.Arbitrary (Path rel typ rep)
  where
  arbitrary = QC.liftArbitrary arbitraryNonEmptyText

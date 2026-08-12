{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-methods:sconcat #-}

module Data.Path.Internal.List
  ( List (List),
    firstTraverse,
  )
where

import "base" Control.Applicative (Applicative, liftA2, pure)
import "base" Control.Category ((.))
import "base" Data.Bifoldable (Bifoldable, bifoldr)
import "base" Data.Bitraversable (Bitraversable, bisequenceA, bitraverse)
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable, foldr)
import "base" Data.Function (($))
import "base" Data.Functor (Functor, fmap, (<$>))
import "base" Data.Kind qualified as Kind
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Ord (Ord)
import "base" Data.Semigroup (Semigroup, stimes, stimesMonoid, (<>))
import "base" Data.Traversable (Traversable, traverse)
import "base" GHC.Generics (Generic)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
-- This has the `Semigroup` and `Monoid` instances for `Mu`.
import "yaya" Yaya.Applied ()
import "yaya" Yaya.Fold
  ( Mu,
    Projectable,
    Recursive,
    Steppable,
    cata,
    embed,
    project,
  )
import "yaya" Yaya.Functor (DFunctor, firstMap)
import "yaya" Yaya.Pattern (XNor (Both, Neither), xnor)

-- | A strict sequence.
--
--  __TODO__: Move this upstream to Yaya.
type List :: Kind.Type -> Kind.Type
newtype List a = List (Mu (XNor a))
  deriving stock (Eq, Generic, Ord, Read, Show)

instance Projectable (->) (List a) (XNor a) where
  project (List mu) = List <$> project mu

instance Recursive (->) (List a) (XNor a) where
  cata φ (List mu) = cata φ mu

instance Steppable (->) (List a) (XNor a) where
  embed = List . embed . fmap (\(List mu) -> mu)

instance Semigroup (List a) where
  List mu <> List mu' = List $ mu <> mu'
  stimes = stimesMonoid

instance Monoid (List a) where
  mempty = List mempty

instance Foldable List where
  foldr f z (List mu) = cata (xnor z f) mu

instance Functor List where
  fmap f (List mu) = List (firstMap f mu)

instance Bifoldable XNor where
  bifoldr f g z = xnor z \a -> f a . (`g` z)

instance Bitraversable XNor where
  bitraverse f g = xnor (pure Neither) \a -> liftA2 Both (f a) . g

-- |
--
--  __TODO__: Move this – "Yaya.Functor" would be a good place, but this depends
--            on things from "Yaya.Fold", which is downstream from that.
firstTraverse ::
  forall d b f a c u.
  ( Recursive (->) (d (b (f c))) (b (f c)),
    Steppable (->) u (b c),
    DFunctor d,
    Bitraversable b,
    Applicative f
  ) =>
  (a -> f c) -> d (b a) -> f u
firstTraverse f = cata @_ @_ @(b (f c)) (fmap embed . bisequenceA) . firstMap f

instance Traversable List where
  traverse f (List mu) = List <$> firstTraverse f mu

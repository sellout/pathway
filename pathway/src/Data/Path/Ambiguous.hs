{-# LANGUAGE Safe #-}
-- __NB__: Because of the nested `Parents` constraints.
{-# LANGUAGE UndecidableInstances #-}

module Data.Path.Ambiguous
  ( Path (..),
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable, foldr, sum)
import "base" Data.Function (($))
import "base" Data.Functor (Functor)
import "base" Data.Kind qualified as Kind
import "base" Data.Ord (Ord, (<=))
import "base" Data.Semigroup ((<>))
import "base" Data.Traversable (Traversable)
import "base" GHC.Generics (Generic, Generic1)
import "base" GHC.Natural (minusNaturalMaybe)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "pathway-internal" Data.Path.Internal (List, Parents, Relativity)
import "pathway-internal" Data.Path.Internal qualified as Path
import "yaya" Yaya.Applied (drop, length)
import "yaya" Yaya.Pattern (Maybe (Nothing))
import "base" Prelude ((+))

type Path :: Relativity -> Kind.Type -> Kind.Type
data Path rel rep = Path
  { parents :: Parents rel,
    directories :: List rep,
    component :: rep
  }
  deriving stock (Foldable, Functor, Generic, Generic1, Traversable)

deriving stock instance (Eq (Parents rel), Eq rep) => Eq (Path rel rep)

deriving stock instance (Ord (Parents rel), Ord rep) => Ord (Path rel rep)

deriving stock instance (Read (Parents rel), Read rep) => Read (Path rel rep)

deriving stock instance (Show (Parents rel), Show rep) => Show (Path rel rep)

instance
  Path.TotalOps
    (Path.Path rel 'Path.Dir rep)
    (Path ('Path.Rel 'False) rep)
    (Path rel rep)
  where
  parent </> child =
    Path
      { parents = Path.parents parent,
        directories = directories child <> Path.directories parent,
        component = component child
      }

instance
  Path.TotalOps
    (Path.Path ('Path.Rel 'True) 'Path.Dir rep)
    (Path ('Path.Rel 'True) rep)
    (Path ('Path.Rel 'True) rep)
  where
  parent </> child =
    Path
      { parents =
          foldr (+) (Path.parents parent)
            . minusNaturalMaybe (parents child)
            . length
            $ Path.directories parent,
        directories =
          directories child <> drop (parents child) (Path.directories parent),
        component = component child
      }

instance
  Path.TotalOps
    (Path.Path ('Path.Rel 'False) 'Path.Dir rep)
    (Path ('Path.Rel 'True) rep)
    (Path ('Path.Rel 'True) rep)
  where
  parent </> child =
    Path
      { parents =
          sum . minusNaturalMaybe (parents child) . length $
            Path.directories parent,
        directories =
          directories child <> drop (parents child) (Path.directories parent),
        component = component child
      }

instance
  Path.PartialOps
    (Path.Path 'Path.Abs 'Path.Dir rep)
    (Path ('Path.Rel 'True) rep)
    (Path 'Path.Abs rep)
  where
  parent </?> child =
    if parents child <= length (Path.directories parent)
      then
        pure
          Path
            { parents = Path.parents parent,
              directories =
                directories child
                  <> drop (parents child) (Path.directories parent),
              component = component child
            }
      else Nothing

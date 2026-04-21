{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Path.Internal.Resolution
  ( Resolution (Unres, Res),
  )
where

import "base" Data.Kind qualified as Kind

type Resolution :: Kind.Type
data Resolution
  = Unres -- "../" allowed anywhere and in any number
  | Res -- "../" - for 'Rel paths: disallowed at the beginning
        -- "../" - for 'Abs paths: disallowed everywhere

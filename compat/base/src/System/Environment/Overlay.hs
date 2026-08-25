{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
-- __NB__: Because of the nested @`Show` (`MP.Token` rep)@ constraints.
{-# LANGUAGE UndecidableInstances #-}

-- | This provides an API similar to "System.Directory", but for Pathway types.
--
--   Some differences:
-- - operations mostly require absolute paths, the “current directory” is not
--   implicit (operations may _return_ relative paths, but they will be relative
--   to an argument). The reason for returning relative paths, is because it’s a
--   total operation (once we separate reparented paths at the type level) to
--   concat the relative path to the path passed in (creating the absolute path
--   that would be returned), but a partial operation to convert a returned
--   absolute path to the same relative path.
-- - there is no `Dir.makeAbsolute`, to do the same thing,
--   @(`</?>` myPath) `<$>` `getCurrentDirectory`@ or similar will work. This
--   just makes all paths explicit, even if they do end up relative to the
--  “current” path. __TODO__: Might be worth removing the idea of a “current”
--   directory altogether?
-- - similarly, no `makeRelativeToCurrentDirectory`
-- - this includes some exception handlers for dealing with filesystem-specific
--   meanings of different `IOError`s.
--
--   One reason for enforcing the absoluteness of paths, is that paths are often
--   reported to users, and often without enough context. This tries to ensure
--   that there is at least a full path available (unless the developer makes an
--   effort to remove it.
#if MIN_VERSION_base(4, 17, 0)
module System.Environment.Overlay
  ( executablePath,
    getExecutablePath,
  )
where
#else
module System.Environment.Overlay
  ( getExecutablePath,
  )
where
#endif

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Exception (throwIO)
import "base" Control.Monad ((=<<))
import "base" Data.Either (either)
import "base" Data.String (String)
import "base" Data.Void (Void)
import "base" System.IO (IO)
import "pathway" Data.Path (Path, Type (File), forgetRelativity)
import "pathway" Data.Path.Relativity (Relativity (Any))
import "this" System.Environment.Thin qualified as Thin
#if MIN_VERSION_base(4, 17, 0)
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe)
import "base" Data.Traversable (traverse)
import "pathway" Data.Path.Relativity (Relativity (Abs))

executablePath :: Maybe (IO (Maybe (Path 'Abs 'File String)))
executablePath =
  (traverse (either throwIO pure) =<<) <$> Thin.executablePath @Void
#endif

getExecutablePath :: IO (Path 'Any 'File String)
getExecutablePath =
  either throwIO (pure . either forgetRelativity forgetRelativity)
    =<< Thin.getExecutablePath @Void

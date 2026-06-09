{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module System.IO.Temp.Pathway
  ( module System.IO.Pathway,
    module System.IO.Temp,
    module System.IO.Temp.Thin,
  )
where

-- These are re-exported via "System.IO.Temp", so mirror that here with the
-- Pathway implementations.
import safe "pathway-compat-base" System.IO.Pathway
  ( openBinaryTempFile,
    openTempFile,
  )
import "temporary" System.IO.Temp hiding
  ( createTempDirectory,
    emptySystemTempFile,
    emptyTempFile,
    getCanonicalTemporaryDirectory,
    openBinaryTempFile,
    openNewBinaryFile,
    openTempFile,
    withSystemTempDirectory,
    withSystemTempFile,
    withTempDirectory,
    withTempFile,
    writeSystemTempFile,
    writeTempFile,
  )
import safe "this" System.IO.Temp.Thin

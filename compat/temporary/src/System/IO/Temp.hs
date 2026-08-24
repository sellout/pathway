{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- Some imports hide identifiers that aren’t defined in some temporary versions.
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module System.IO.Temp
  ( module System.IO.DropIn,
    module System.IO.Temp,
    module System.IO.Temp.Thin,
    module System.IO.Temp.Overlay,
  )
where

-- These are re-exported via "System.IO.Temp", so mirror that here with the
-- Pathway implementations.
import safe "pathway-compat-base" System.IO.DropIn
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
import safe "this" System.IO.Temp.Overlay
import safe "this" System.IO.Temp.Thin hiding
  ( createTempDirectory,
    emptySystemTempFile,
    emptyTempFile,
    getCanonicalTemporaryDirectory,
    openNewBinaryFile,
    withSystemTempDirectory,
    withSystemTempFile,
    withTempDirectory,
    withTempFile,
    writeSystemTempFile,
    writeTempFile,
  )

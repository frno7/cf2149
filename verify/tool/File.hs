-- SPDX-License-Identifier: GPL-2.0
-- Copyright (C) 2025 Fredrik Noring

module File
    ( readWriteFile
    ) where

import System.Directory (renameFile)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

errorExit :: FilePath -> String -> IO a
errorExit filePath message = do
    hPutStr stderr $ filePath ++ ":" ++ message
    exitFailure

readWriteFile :: FilePath -> FilePath
              -> (String -> Either String String) -> IO ()
readWriteFile ifp ofp f = do
    r <- readFile ifp
    w <- case f r of
        Left  e -> errorExit ifp e
        Right s -> pure $ s
    let tmp = ofp ++ ".tmp"
    writeFile tmp w >> renameFile tmp ofp

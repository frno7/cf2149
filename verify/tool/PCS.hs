{-# LANGUAGE RecordWildCards #-}
-- SPDX-License-Identifier: GPL-2.0
-- Copyright (C) 2025 Fredrik Noring

module PCS
    ( fromPcs
    , Bdc (..)
    , Register
    , Command (..)
    ) where

import Data.Char (toUpper)

import Logic

data Bdc       -- Bus direction, bus control 2, 1
               -- BDIR BC2 BC1 Mode
    = NACT     --   0   0   0  Inactive
    | ADAR     --   0   0   1  Latch address
    | IAB      --   0   1   0  Inactive
    | DTB      --   0   1   1  Read from PSG
    | BAR      --   1   0   0  Latch address
    | DW       --   1   0   1  Inactive
    | DWS      --   1   1   0  Write to PSG
    | INTAK    --   1   1   1  Latch address
    deriving (Eq, Enum, Show, Read)

data Register
    = PLO_A    --  0  Period of channel A fine tone
    | PHI_A    --  1  Period of channel A rough tone
    | PLO_B    --  2  Period of channel B fine tone
    | PHI_B    --  3  Period of channel B rough tone
    | PLO_C    --  4  Period of channel C fine tone
    | PHI_C    --  5  Period of channel C rough tone
    | PNOISE   --  6  Period of noise
    | IOMIX    --  7  I/O port and mixer settings
    | LEVEL_A  --  8  Level of channel A
    | LEVEL_B  --  9  Level of channel B
    | LEVEL_C  -- 10  Level of channel C
    | PLO_ENV  -- 11  Period of envelope fine
    | PHI_ENV  -- 12  Period of envelope rough
    | SHAPE    -- 13  Shape of envelope
    | DATA_A   -- 14  Data of I/O port A
    | DATA_B   -- 15  Data of I/O port B
    deriving (Eq, Enum, Show, Read)

data Command
    = WAIT     Integer
    | RESET_L  Logic
    -- Data/address bus
    | DA       Vector_Logic
    | REG      Register
    -- Bus control
    | A9_L     Logic
    | A8       Logic
    | BDIR     Logic
    | BC2      Logic
    | BC1      Logic
    | BDC      Bdc
    -- Clock divisor control (/8 on high, /16 on low)
    | SELECT_L Logic
    -- Port A
    | IOA      Vector_Logic
    -- Port B
    | IOB      Vector_Logic
    deriving (Eq, Show, Read)

toCommand :: String -> Command
toCommand ('#':s) = WAIT $ read s
toCommand s = read $ r <$> s where r c | c == '=' = ' ' | otherwise = toUpper c

decomment :: String -> String
decomment = unwords . takeWhile (/= "--") . words

fromPcs :: String -> Either String [Command]
fromPcs
    = (Right)
    . fmap toCommand
    . concat
    . fmap (words . decomment)
    . lines

{-# LANGUAGE RecordWildCards #-}
-- SPDX-License-Identifier: GPL-2.0
-- Copyright (C) 2025 Fredrik Noring

module PVS
    ( toPvs
    ) where

import Control.Exception (assert)
import Data.List.Split (splitPlaces)

import Logic
import PCS

split3 :: Int -> Int -> [a] -> [[a]]
split3 i j xs = splitPlaces [n - i - 1, 1 + i - j, j] xs where n = length xs

fromVector :: Int -> Int -> Vector_Logic -> Integer
fromVector i j (Vector v) =
    assert (length v >= i + 1 && i >= j && i >= 0)
    assert (length x + length w + length y == length v)
    foldl (\n k -> n + n + k) 0 w
    where [x, w, y] = fmap (toInteger . fromEnum . (== L_1)) <$> split3 i j v

toVector :: Int -> Integer -> Vector_Logic
toVector w n = Vector [ [L_0, L_1] !! bit i | i <- [w-1, w-2 .. 0] ]
    where bit i = fromInteger $ div n (2 ^ i) `mod` 2

slice :: Vector_Logic -> Int -> Int -> Vector_Logic -> Vector_Logic
slice (Vector w) i j (Vector m) =
    assert (length z == j)
    assert (length m + j == i + 1)
    assert (length x + i + 1 == length w)
    assert (length x + length m + length z == length w)
    Vector $ x <> m <> z where [x, _, z] = split3 i j w

-- FIXME: Define tree:
-- CYCLE    : 64
-- RESET_L  :  1
-- SELECT_L :  1
-- BDC BDIR :  1
-- BDC BC2  :  1
-- BDC BC1  :  1
-- A98 A9_L :  1
-- A98 A8   :  1
-- DA       :  4
-- DA REG   :  4
-- IOA      :  8
-- IOB      :  8

eval :: Vector_Logic -> Command -> Vector_Logic
eval v cmd =
    case cmd of
    BDC       bdc -> w 28 26 $ toVector  3 $ toInteger $ fromEnum bdc
    REG       reg -> w 23 16 $ toVector  8 $ toInteger $ fromEnum reg
    WAIT        c -> w 94 31 $ toVector 64 $ c + fromVector 94 31 v
    RESET_L     d -> b    30 $ d
    SELECT_L    d -> b    29 $ d
    BDIR        d -> b    28 $ d
    BC2         d -> b    27 $ d
    BC1         d -> b    26 $ d
    A9_L        d -> b    25 $ d
    A8          d -> b    24 $ d
    DA          d -> w 23 16 $ d
    IOA         d -> w 15  8 $ d
    IOB         d -> w  7  0 $ d
    where w h l d = slice v h l $ d
          b   p d = slice v p p $ Vector [d]

evals :: Vector_Logic -> [Command] -> [Vector_Logic]
evals v [] = [v]
evals v (cmd:cmds) = [ v | WAIT _ <- [cmd] ] <> evals (eval v cmd) cmds

toPvs :: [Command] -> Either String String
toPvs
    = (Right)
    . unlines
    . fmap show
    . evals (Vector $ replicate 64 L_0 <> replicate 31 L_X)

{-# LANGUAGE RecordWildCards #-}
-- SPDX-License-Identifier: GPL-2.0
-- Copyright (C) 2025 Fredrik Noring

module Logic
    ( Logic (..)
    , Vector (..)
    , Vector_Logic
    , toLogic
    , fromLogic
    ) where

import Data.Char (toLower)
import Data.Maybe (fromJust)
import Data.List (findIndex, singleton)

data Logic
    = L_U      -- U  Uninitialized
    | L_X      -- X  Forcing  Unknown
    | L_0      -- 0  Forcing  0
    | L_1      -- 1  Forcing  1
    | L_Z      -- Z  High Impedance
    | L_W      -- W  Weak     Unknown
    | L_L      -- L  Weak     0
    | L_H      -- H  Weak     1
    | L_D      -- -  Don't care
    deriving (Eq);

toLogic   = tr "ux01zwlh-" [L_U, L_X, L_0, L_1, L_Z, L_W, L_L, L_H, L_D]
fromLogic = tr [L_U, L_X, L_0, L_1, L_Z, L_W, L_L, L_H, L_D] "ux01zwlh-"
tr m w x  = (w !!) $ fromJust $ findIndex (== x) m

instance Show Logic where show x = [toLower $ fromLogic x]
instance Read Logic where
    readsPrec p (' ':s) = readsPrec p s
    readsPrec _ (c:s) = [(toLogic $ toLower c, s)]
    readsPrec _ s     = error $ "Malformed logic: '" ++ s ++ "'"

data Vector a = Vector [a] deriving (Eq)
type Vector_Logic = Vector Logic

instance Show a => Show (Vector a) where
    show (Vector xs) = concat $ show <$> xs
instance Read a => Read (Vector a) where
    readsPrec p (' ':s) = readsPrec p s
    readsPrec _ s = [(Vector $ read . singleton <$> s, "")]

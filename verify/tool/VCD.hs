{-# LANGUAGE RecordWildCards #-}
-- SPDX-License-Identifier: GPL-2.0
-- Copyright (C) 2025 Fredrik Noring

module VCD
    ( toVcd
    ) where

import Data.Char (toLower)
import Data.List (nub, sort, sortBy, unfoldr)
import Data.Maybe (fromJust)
import Data.Tree
import Numeric (showBin)

import Signal

type Code = String
type SignalCode = (Signal [String], Code)

data SignalTree
    = NodeName   String
    | NodeSignal SignalCode
    deriving (Eq, Show)

toSignal :: Event [a] (SignalValue (Signal [a]) v) -> Signal [a]
toSignal (Event eventName _ (SignalValue s@Signal {..} _)) =
    s { signalName = eventName <> signalName }

code :: Int -> String
code = reverse . unfoldr (\n -> case divMod n 26 of
    (-1, _) -> Nothing
    ( j, i) -> Just (['a'..] !! i, j - 1))

insertPath :: SignalCode -> [String] -> [Tree SignalTree] -> [Tree SignalTree]
insertPath sc [] = (:) (Node (NodeSignal sc) [])
insertPath sc (p:ps) = go
    where go [] = [Node (NodeName p) (insertPath sc ps [])]
          go (n@(Node x xs) : ns)
              | NodeName p == x = Node x (insertPath sc ps xs) : ns
              | otherwise = n : go ns

insertPaths :: [Tree SignalTree] -> [([String], Signal [String], Code)]
            -> [Tree SignalTree]
insertPaths = foldl (\t (ps, s, c) -> insertPath (s, c) ps t)

tree :: [SignalCode] -> [Tree SignalTree]
tree sc = insertPaths [] [ (init $ signalName s, s, c) | (s, c) <- sc ]

scopes :: Tree SignalTree -> [String]
scopes (Node (NodeName n) ts)
    = [ "$scope module " ++ n ++ " $end" ]
   ++ concat (fmap scopes ts)
   ++ [ "$upscope $end" ]
scopes (Node (NodeSignal (Signal {..}, c)) ts) =
      [ "$var reg " ++ show signalWidth
             ++ " " ++ c ++ " " ++ last signalName ++ range signalWidth
             ++ " $end"] ++ concat (fmap scopes ts)
   where range 1 = ""
         range n = "[" ++ show (n - 1) ++ ":0]"

header :: TimeUnit -> [SignalCode] -> [String]
header tu sc -- FIXME: Autotimescale 1, 10, 100
    = ["$timescale 1 " ++ (toLower <$> show tu) ++ " $end"]
   ++ concat (scopes <$> tree sc)
   ++ ["$enddefinitions $end"]

timeUnits :: [Event a b] -> [TimeUnit]
timeUnits = sort . nub . fmap (timeUnit . eventTime)

time :: TimeUnit -> Time -> Integer
time tu Time {..} =
    div (timeCycle * timeDelta * (toInteger $ fromEnum timeUnit))
        (toInteger $ fromEnum tu)

signalValue :: Integral v => Int -> v -> String
signalValue 1 v =        showBin v ""
signalValue _ v = "b" ++ showBin v "" ++ " "

values :: Integral v => TimeUnit -> [SignalCode]
       -> [Event [String] (SignalValue (Signal [String]) v)] -> [String]
values tu sc se = f (-1) se
    where f _ [] = []
          f t es@(e:_) | t /= t' = ("#" ++ show t'):f t' es
              where t' = time tu $ eventTime e
          f t (e:es) = g (eventName e) (event e):f t es
          g p (SignalValue Signal {..} v) =
                 signalValue signalWidth v
              ++ fromJust (lookup (Signal (p <> signalName) signalWidth) sc)

toVcd :: (Eq v, Show v, Integral v)
      => [Event [String] [(SignalValue (Signal [String]) v)]]
      -> Either String String
toVcd events = Right $ unlines $ header tu sc <> values tu sc se
    where tu = last $ timeUnits es
          sc = zip (nub (toSignal <$> se)) (code <$> [0..])
          se = concat (fmap singleEvents es)
          es = sortBy (\a b -> compare (et a) (et b)) events
          et (Event _ t _) = time S t

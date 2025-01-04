{-# LANGUAGE RecordWildCards #-}
-- SPDX-License-Identifier: GPL-2.0
-- Copyright (C) 2025 Fredrik Noring

module Signal
    ( Signal (..)
    , SignalValue (..)
    , TimeUnit (..)
    , Time (..)
    , Event (..)
    , singleEvents
    ) where

import Data.Maybe (fromJust)
import Data.Tuple (swap)

data Signal n
    = Signal
    { signalName  :: n
    , signalWidth :: Int
    } deriving (Eq, Ord, Show);

data SignalValue s v = SignalValue s v deriving (Eq, Ord, Show)

data TimeUnit
    =  S  --      seconds
    | MS  -- milliseconds
    | US  -- microseconds
    | NS  --  nanoseconds
    | PS  --  picoseconds
    | FS  -- femtoseconds
    deriving (Eq, Ord, Show, Read)

timeUnits = [ (u, 1000^e) | (u, e) <- zip [ S, MS, US, NS, PS, FS ] [0..] ]

instance Enum TimeUnit where
    fromEnum u = fromJust $ lookup u timeUnits
    toEnum   n = fromJust $ lookup n $ swap <$> timeUnits

data Time
    = Time
    { timeCycle :: Integer
    , timeDelta :: Integer
    , timeUnit  :: TimeUnit
    } deriving (Eq, Ord, Show);

data Event n e
    = Event
    { eventName :: n
    , eventTime :: Time
    , event     :: e
    } deriving (Eq, Show);

singleEvents :: Event a [SignalValue s v] -> [Event a (SignalValue s v)]
singleEvents Event {..} = [ Event eventName eventTime s | s <- event ]

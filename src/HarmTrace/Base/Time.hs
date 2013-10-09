{-# LANGUAGE TypeSynonymInstances             #-}
{-# LANGUAGE FlexibleInstances                #-}
{-# LANGUAGE DeriveFunctor                    #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.MusicTime
-- Copyright   :  (c) 2012--2013 W. Bas de Haas and Jose Pedro Magalhaes
-- License     :  LGPL-3
--
-- Maintainer  :  bas@chordify.net, dreixel@chordify.net 
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A set of types and classes for representing musical time, mainly
-- (but not necessarily) in the context of recognising chords 
-- from an arbitrary audio source.
--------------------------------------------------------------------------------

module HarmTrace.Base.Time (
   
   NumData
  -- ** Representing musical time
  , Timed (..)
  , Beat (..)
  , BeatTime (..) 

  -- * Functions
  -- ** Data access
  , timed
  , timedBT
  , getBeatTime 
  , getBeat 
  , onset
  , offset
  , duration
  , timeComp
  , setData
  
  -- ** Type conversion and other utilities
  -- , fromDurations
  , concatTimed
  , splitTimed
  , nextBeat
  , prevBeat 
  , dropTimed
  , timeStamp
  , beat 
  , pprint
  , prettyPrint

) where
             
import Data.List                      ( intercalate )

-- | When reducing and expanding 'Timed' types there might be rounding
-- errors in the floating point time stamps. The 'roundingError' parameter
-- sets the acceptable rounding error that is used in the comparison of 
-- time stamps (e.g. see 'timeComp')
roundingError :: NumData 
roundingError = 0.0001 -- = one milisecond  


-- | A type synonym is defined for our main numerical representation, this 
-- allows us to easily change the precision.
type NumData = Double

--------------------------------------------------------------------------------
-- High-level structure
--------------------------------------------------------------------------------


-- | A datatype that wraps around an (musical) datatype, adding information 
-- about the musical time to this datatype. Musical time is stored as 
-- a list of 'BeatTime' time stamps that can optionally be augmented
-- with information about the 'Beat' position of the particular time stamp 
-- inside the bar.
data Timed a = Timed { -- | Returns the contained datatype 
                               getData :: a 
                               -- | Returns the list of TimeStamps
                             , getTimeStamps :: [BeatTime]
                             } deriving (Functor, Show, Eq)

-- | For now, we fix the number of available beats to four, because this is also
-- hard-coded into the bar and beat-tracker.
data Beat = One | Two | Three | Four | NoBeat deriving (Eq, Enum)

--------------------------------------------------------------------------------
-- Instances of high-level data structure
--------------------------------------------------------------------------------

  
instance Show Beat where
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show NoBeat = "x"

instance Show BeatTime where
  show (BeatTime t bt) = '(' : show t ++ ", " ++ show bt ++ ")"
  show (Time t)       = '(' : show t ++ ")"

    
--------------------------------------------------------------------------------
-- numerical data representation
--------------------------------------------------------------------------------


-- TODO Rename to BeatTime
-- | Represents a musical time stamp, which is a 'NumData' possibly augmented
-- with a 'Beat' denoting the position of the time stamp within a bar.
data BeatTime = BeatTime NumData Beat
             | Time    NumData      deriving Eq

-- we compare based on the timestamp only
instance Ord BeatTime where
  compare a b = compare (timeStamp a) (timeStamp b)

--------------------------------------------------------------------------------
-- Some type conversion utilities
--------------------------------------------------------------------------------
{-
fromDurations :: NumData -> [(a,NumData)] -> [Timed a]
fromDurations z td = foldl' step [] td where

  step :: [Timed a] -> (a, NumData) ->  [Timed a]
  step []                         (a, x) = [TimedData x [Time z, Time x]]
  step [Timed _ [Time _, Time o]] (a, x) = [TimedData x [Time o, Time x]]
-}

-- | alternative 'Timed' constructor
timed :: a -> NumData -> NumData -> Timed a
timed d x y = Timed d [Time x, Time y]

-- | alternative 'Timed' constructor
timedBT :: a -> BeatTime -> BeatTime -> Timed a
timedBT d x y = Timed d [x, y]

-- | concatenates the 'BeatTime' timestamps of two 'Timed's and 
-- creates a new 'Timed' that stores the first argument. 
concatTimed :: a -> Timed a -> Timed a -> Timed a
concatTimed dat (Timed _ ta) (Timed _ tb) = 
  Timed dat (mergeBeatTime ta tb) where

  mergeBeatTime :: [BeatTime] -> [BeatTime] -> [BeatTime]
  mergeBeatTime [] b = b
  mergeBeatTime a [] = a
  mergeBeatTime a b = case timeComp (timeStamp . last $ a) 
                                    (timeStamp . head $ b) of
    GT -> error ("HarmTrace.Base.MusicTime.mergeBeatTime: " ++
                 "cannot merge BeatTimes "  ++ show a ++ " and " ++ show b)
    EQ -> a ++ tail b -- do not include the same timestamp twice
    LT -> a ++ b  

-- | Splits a 'Timed' in two 'Timed's at the specified position. If
-- the position is out of range, an error is thrown.
--
-- >>> splitTimed (Timed "x" [Time 2, Time 5]) 4
-- >>> ( Timed {getData = "x", getTimeStamps = [(2.0),(4.0)]}
-- >>> , Timed {getData = "x", getTimeStamps = [(4.0),(5.0)]} )
splitTimed :: Show a => Timed a -> NumData -> (Timed a, Timed a)
splitTimed td@(Timed d t) s 
  | s > onset td = case span ((< s) . timeStamp) t of
                    (_, []) -> e
                    (x, y ) -> ( Timed d (x ++ [Time s])
                               , Timed d (Time s : y   ))
  | otherwise    = e
      where e = error ( "HarmTrace.Base.MusicTime.splitTimed: Timestamp " 
                      ++ show s ++ " not in range of Timed: " ++ show td) 
  
    
-- | compares to 'NumData' timestamps taking a rounding error 'roundingError'
-- into account.
timeComp :: NumData -> NumData -> Ordering
timeComp a b 
 | a > (b + roundingError) = GT
 | a < (b - roundingError) = LT
 | otherwise               = EQ

-- | wraps a datatype in 'Timed'
setData :: Timed a -> b -> Timed b
setData td d = td {getData = d}

-- | Returns the start time stamp
getBeatTime :: Timed a -> BeatTime
getBeatTime td = case getTimeStamps td of
  []    -> error "HarmTrace.Base.MusicTime.getBeatTime: no timestamps are stored"
  (h:_) -> h

-- | Returns the start 'Beat'
getBeat :: Timed a -> Beat
getBeat = beat . getBeatTime 

-- | Returns the 'NumData' timestamp, given a 'BeatTime'
timeStamp :: BeatTime -> NumData
timeStamp (BeatTime t _bt) = t
timeStamp (Time    t    ) = t  

-- | Returns the 'NumData' timestamp, given a 'BeatTime'
beat :: BeatTime -> Beat
beat (BeatTime _t bt) = bt
beat (Time    _t   ) = NoBeat

-- | Returns the onset time stamp
onset :: Timed a -> NumData
onset = timeStamp . getBeatTime 

-- | Returns the offset time stamp
offset :: Timed a -> NumData
offset td = case getTimeStamps td of
  []  -> error "HarmTrace.Base.MusicTime.offset: no timestamps are stored"
  l   -> timeStamp . last $ l

-- | Returns the duration of 'Timed'
duration :: Timed a -> NumData
duration td = offset td - onset td

-- TODO: replace by ad-hoc enum instance?
-- | returns the next beat, e.g. @ nextBeat Two = Three @. 
-- Following the (current) definition of 'Beat', we still assume 4/4, in the 
-- future this function should also have the meter as an argument.
nextBeat, prevBeat :: Beat -> Beat 
nextBeat Four = One
nextBeat b    = succ b

-- | returns the previous 'Beat', similar to 'prevBeat'.
prevBeat One  = Four
prevBeat b    = pred b

-- | drops the time (with or without 'Beat') information of a list 
-- 'Timed' data structure 
dropTimed :: [Timed a] -> [a]
dropTimed = map getData

-- | Pretty prints a list of 'Timed's, one per line
prettyPrint :: Show a => [Timed a] -> String
prettyPrint = intercalate "\n" . map pprint where

-- | Pretty prints a single 'Timed'
pprint :: Show a => Timed a -> String
pprint (Timed d [ ]) = "not set - not set: " ++ show d
pprint (Timed d [x]) = show x ++" - not set: " ++ show d
pprint (Timed d ts ) = show (head ts) ++ " - " ++ show (last ts) 
                                          ++ ": "  ++ show d

{-# LANGUAGE TypeSynonymInstances             #-}
{-# LANGUAGE FlexibleInstances                #-}
{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE DeriveFunctor                    #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.MusicTime
-- Copyright   :  (c) 2012--2015 W. Bas de Haas and Jose Pedro Magalhaes
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
  , MeterKind (..)

  -- * Functions
  -- ** Data access
  , timed
  , timedBT
  , onBeatTime
  , offBeatTime
  , onBeat
  , offBeat
  , onset
  , offset
  , duration
  , setData
  , getEndTime

  -- ** Type conversion and other utilities
  -- , fromDurations
  , mergeTimed
  , mergeTimedWith
  , expandTimed
  , concatTimed
  , splitTimed
  , setMeterKind
  , updateBeats
  , splitPickup -- remove
  , nextBeat
  , prevBeat
  , lastBeat
  , dropTimed
  , timeStamp
  , timeComp
  , roundingError
  , beat
  , pprint
  , prettyPrint

) where

import Data.List                      ( intercalate, mapAccumL )
import Text.Printf                    ( printf )

-- | When reducing and expanding 'Timed' types there might be rounding
-- errors in the floating point time stamps. The 'roundingError' parameter
-- sets the acceptable rounding error that is used in the comparison of
-- time stamps (e.g. see 'timeComp')
roundingError :: BeatTime
roundingError = 0.0001 -- = one millisecond


-- | A type synonym is defined for our main numerical representation, this
-- allows us to easily change the precision.
-- type NumData = Float

--------------------------------------------------------------------------------
-- High-level structure
--------------------------------------------------------------------------------

type Timed a = Fractional t => Timed' a t

-- | A datatype that wraps around an (musical) datatype, adding information
-- about the musical time to this datatype. Musical time is stored as
-- a list of 'BeatTime' time stamps that can optionally be augmented
-- with information about the 'Beat' position of the particular time stamp
-- inside the bar.
data Timed' a t = Timed { getData :: a
                        -- | Returns the contained datatype
                        , getTimeStamps :: [BeatTime t]
                        -- | Returns the list of TimeStamps
                        } deriving (Functor, Show, Eq)

-- | For now, we fix the number of available beats to four, because this is also
-- hard-coded into the bar and beat-tracker.
data Beat = One | Two | Three | Four | NoBeat deriving (Eq, Ord, Enum)

-- | Having a high-level representation of a musical meter: 'Duple' is
-- counted in two and 'Triple' in three.
data MeterKind = Duple | Triple deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------
-- Instances of high-level data structure
--------------------------------------------------------------------------------


instance Show Beat where
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show NoBeat = "x"

instance Fractional t => Show (BeatTime t) where
  show (BeatTime t bt) = printf ("(%.2f, " ++ show bt ++ ")") t
  show (Time t)        = printf  "(%.2f)" t


--------------------------------------------------------------------------------
-- numerical data representation
--------------------------------------------------------------------------------

-- type BeatTime = BeatAndTime Float

-- | Represents a musical time stamp, which is a 'NumData' possibly augmented
-- with a 'Beat' denoting the position of the time stamp within a bar.
data BeatTime a = BeatTime a Beat
                | Time     a       deriving Eq

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
timed :: Fractional t => a -> t -> t -> Timed a
timed d x y = Timed d [Time x, Time y]

-- | alternative 'Timed' constructor
timedBT :: a -> BeatTime -> BeatTime -> Timed a
timedBT d x y = Timed d [x, y]

-- | concatenates the 'BeatTime' timestamps of two 'Timed's and
-- creates a new 'Timed' that stores the first argument.
-- N.B. this function uses 'timeComp' to allow for very small timing deviations
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


-- | the inverse of 'mergeTimed', expanding the list 'Timed' elements to all
-- timestamps stored in the 'getTimeStamps' list. N.B.
--
-- >>> expandTimed (mergeTimed x) = x :: [Timed a]
--
-- also,
--
-- >>> (expandTimed cs) = cs
--
-- and,
--
-- >>> mergeTimed (mergeTimed (mergeTimed cs)) = (mergeTimed cs)
--
-- hold. This has been tested on the first tranche of 649 Billboard songs.
expandTimed :: [Timed a] -> [Timed a]
expandTimed = concatMap replic where

  replic :: Timed a -> [Timed a]
  replic (Timed c ts) = let reps = repeat c
                        in  zipWith3 timedBT (c : reps) ts (tail ts)

-- | merges consecutive 'Timed' values that store the same element (using
-- ('(==)'). For example:
--
-- >>> mergeTimed [timed "c" 0 1, timed "c" 1 2, timed "d" 3 4, timed "d" 4 5, timed "e" 5 6]
-- >>> [Timed {getData = "c", getTimeStamps = [(0.0),(1.0),(2.0)]}
-- >>> ,Timed {getData = "d", getTimeStamps = [(3.0),(4.0),(5.0)]}
-- >>> ,Timed {getData = "e", getTimeStamps = [(5.0),(6.0)]}]
--
mergeTimed :: Eq a => [Timed a] -> [Timed a]
mergeTimed = mergeTimedWith (==)

-- | Does exactly what 'mergeTimed' does, but allows for a custom equality
-- function
mergeTimedWith :: forall a. Eq a => (a -> a -> Bool) -> [Timed a] -> [Timed a]
mergeTimedWith eq = foldr groupT [] where

   groupT :: Eq a => Timed a -> [Timed a] -> [Timed a]
   groupT c [] = [c]
   groupT tc@(Timed c _ ) (th@(Timed h _ ) : t)
     | c `eq` h  = concatTimed c tc th : t
     | otherwise = tc : th : t

-- | Splits a 'Timed' in two 'Timed's at the specified position. If
-- the position is out of range, an error is thrown.
--
-- >>> splitTimed (Timed "x" [Time 2, Time 5]) 4
-- >>> ( Timed {getData = "x", getTimeStamps = [(2.0),(4.0)]}
-- >>> , Timed {getData = "x", getTimeStamps = [(4.0),(5.0)]} )
splitTimed :: (Show a, Fractional t) => Timed a t -> t -> (Timed a, Timed a)
splitTimed td@(Timed d t) s
  | s > onset td = case span ((< s) . timeStamp) t of
                    (_, []) -> e
                    (x, y ) -> ( Timed d (x ++ [Time s])
                               , Timed d (Time s : y   ))
  | otherwise    = e
      where e = error ( "HarmTrace.Base.MusicTime.splitTimed: Timestamp "
                      ++ show s ++ " not in range of Timed: " ++ show td)

-- | Changes the internal 'MeterKind' of a 'Timed' sequence. We assume
-- that meter changes do nog occur.
setMeterKind :: MeterKind -> [Timed a] -> [Timed a]
setMeterKind _ [] = []
setMeterKind mk x =
  let (pu, cs) = splitPickup x -- will expand the chords

      srtpu    = (iterate (prevBeat mk) (lastBeat mk)) !! (pred . length $ pu)
      srt      = onBeat (head cs)

  in  (updateBeats mk srtpu pu) ++
      (updateBeats mk srt   cs)

-- TODO: maybe setBeats is a better name..?
-- | applies updateBeat to a list
--
-- >>> updateBeats Triple Three [ timedBT "a" (BeatTime 0 Three) (BeatTime 1 Four)
-- >>>                          , timedBT "a" (BeatTime 1 Four) (BeatTime 2 One)
-- >>>                          , Timed "a" [ BeatTime 2 One, BeatTime 3 Two
-- >>>                                      , BeatTime 4 Three, BeatTime 5 Four]]
-- >>> [Timed {getData = "a", getTimeStamps = [(0.0, 3),(1.0, 1)]}
-- >>> ,Timed {getData = "a", getTimeStamps = [(1.0, 1),(2.0, 2)]}
-- >>> ,Timed {getData = "a", getTimeStamps = [(2.0, 2),(3.0, 3),(4.0, 1),(5.0, 2)]}]
updateBeats :: MeterKind -> Beat -> [Timed a] -> [Timed a]
updateBeats _      _      [] = []
updateBeats Triple Four   cs = updateBeats Triple One cs
updateBeats _      NoBeat cs = cs
updateBeats mk s cs = snd . mapAccumL f s $ cs

  where f :: Beat -> Timed a -> (Beat, Timed a)
        f a b = let x = updateBeat mk a b in (offBeat x, x)

-- | Update the 'Beat's in 'Timed' data given a 'MeterKind' and a
-- starting beat:
--
-- >>> updateBeat Triple Two (Timed "c" [ BeatTime 0 Three
-- >>>                                  , BeatTime 1 Four
-- >>>                                  , BeatTime 2 One])
-- >>> Timed {getData = "c", getTimeStamps = [(0.0, 2),(1.0, 3),(2.0, 1)]}
updateBeat :: MeterKind -> Beat -> Timed a -> Timed a
updateBeat mk strt (Timed d ts) =
  Timed d . zipWith BeatTime (map timeStamp ts)
          . iterate (nextBeat mk) $ strt

-- |
-- N.B. calls 'expandTimed' before splitting
splitPickup :: [Timed a] -> ([Timed a], [Timed a])
splitPickup cs = case span (\t -> (onBeat t) /= One) . expandTimed $ cs of
                  (x, []) -> ([], x) -- in case we have a very short sequence
                                     -- don't treat it as a pickup
                  y       -> y


-- | compares to 'NumData' timestamps taking a rounding error 'roundingError'
-- into account.
timeComp :: Fractional t => t -> t -> Ordering
timeComp a b
 | a > (b + roundingError) = GT
 | a < (b - roundingError) = LT
 | otherwise               = EQ

-- | wraps a datatype in 'Timed'
setData :: Timed a -> b -> Timed b
setData td d = td {getData = d}

-- | Returns the 'NumData' timestamp, given a 'BeatTime'
timeStamp :: Fractional t -> BeatTime -> t
timeStamp (BeatTime t _bt) = t
timeStamp (Time     t    ) = t

-- | Returns the 'NumData' timestamp, given a 'BeatTime'
beat :: BeatTime -> Beat
beat (BeatTime _t bt) = bt
beat (Time     _t   ) = NoBeat

-- | Returns the start 'BeatTime'
onBeatTime :: Timed a -> BeatTime
onBeatTime td = case getTimeStamps td of
  []    -> error "HarmTrace.Base.Time.onBeatTime: no timestamps are stored"
  (h:_) -> h

-- | Returns the offset time stamp
offBeatTime :: Timed a -> BeatTime
offBeatTime td = case getTimeStamps td of
  []  -> error "HarmTrace.Base.Time.offBeatTime: no timestamps are stored"
  l   -> last l

-- | Returns the start 'Beat'
onBeat :: Timed a -> Beat
onBeat = beat . onBeatTime

-- | Returns the offset time stamp
offBeat :: Timed a -> Beat
offBeat = beat . offBeatTime

-- | Returns the onset time stamp
onset :: Fractional t => Timed a -> t
onset = timeStamp . onBeatTime

-- | Returns the offset time stamp
offset :: Fractional t => Timed a -> t
offset = timeStamp . offBeatTime

-- | Given a list of 'Timed' values, returns the end time of the latest element
-- in the list.
getEndTime :: Fractional t => [Timed a] -> t
getEndTime [] = error "getEndTime: empty list"
getEndTime l  = offset . last $ l

-- | Returns the duration of 'Timed'
duration :: Fractional t => Timed a -> t
duration td = offset td - onset td

-- TODO: replace by ad-hoc enum instance?
-- | returns the next beat, e.g. @ nextBeat Two = Three @.
-- Following the (current) definition of 'Beat', we still assume 4/4, in the
-- future this function should also have the meter as an argument.
-- N.B. @ nextBeat Four = One @
nextBeat :: MeterKind -> Beat -> Beat
nextBeat Duple  Four   = One
nextBeat Triple Three  = One
nextBeat _      NoBeat = error "HarmTrace.Base.Time.nextBeat: nextBeat applied toNoBeat"
nextBeat _      b      = succ b

-- | returns the previous 'Beat', similar to 'prevBeat'.
prevBeat :: MeterKind -> Beat -> Beat
prevBeat Duple  One  = Four
prevBeat Triple One  = Three
prevBeat _      NoBeat = error "HarmTrace.Base.Time.prevBeat: nextBeat applied toNoBeat"
prevBeat _      b    = pred b

-- | returns the last 'Beat' of the 'MeterKind'
--
-- >>> lastBeat Duple
-- >>> Four
lastBeat :: MeterKind -> Beat
lastBeat Triple = Three
lastBeat Duple  = Four

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

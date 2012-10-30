{-# LANGUAGE TypeSynonymInstances             #-}
{-# LANGUAGE FlexibleInstances                #-}
{-# LANGUAGE DeriveFunctor                    #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.MusicTime
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A set of types and classes for representing musical time, mainly
-- (but not necessarily) in the context of recognising chords 
-- from an arbitrary audio source.
--------------------------------------------------------------------------------

module HarmTrace.Base.MusicTime (
  -- * The 'Timed' class
    -- Timed (..)
  
  -- * Datatypes
  -- ** Types for representing Chords and their probabilities
  -- , ChordBeatAnnotation 
    ChordAnnotation 
  , ProbChordSeg (..)
  , ProbChord (..)
  , ChordCand (..)
  
  -- ** Representing musical time
  , TimedData (..)
  -- , BeatTimedData (..)
  , Beat (..)
  , BeatBar (..)
  , BeatBarTrackData 
  , NumData 
  
  -- ** Representing raw audio data 
  , AudioFeat (..)
  , ChordinoData 
  , ChordinoLine (..)
  , KeyStrengthData 
  , BeatTrackerData 
  , BeatChroma 
  , ChordStruct 

  -- * Functions
  -- ** Data access
  , getBeatBar 
  , getBeat 
  , onset
  , offset
  , setData
  -- ** Type conversion and other utilities
  , getBeatTrack
  , concatTimedData
  , nextBeat
  , prevBeat 
  , updateTPChord
  , dropProb
  , dropTimed
  , timeStamp
  , beat 

  -- ** miscellaneous
  , chromaPC
)where
             
import HarmTrace.Base.MusicRep
-- import Text.Printf (printf)
-- import Control.DeepSeq


-- | A type synonym is defined for our main numerical representation, this 
-- allows us to easily change the precision.
type NumData = Double

--------------------------------------------------------------------------------
-- High-level structure
--------------------------------------------------------------------------------

-- | A chord annotation consists of a
-- list with chords and segment boundaries.
type ChordAnnotation = [TimedData ProbChord]

-- | A datatype that wraps around an arbitrary datatype, adding (in this order)
-- a 'Beat', an onset, and an offset.
data TimedData a = TimedData { -- | Returns the contained datatype 
                               getData :: a 
                               -- | Returns the 'Beat'
                             -- , getBeat :: Beat 
                               -- | Returns the onset timestamp
                             -- , onset   :: NumData
                               -- | Returns the offset timestamp
                             -- , offset  :: NumData 
                               -- | Returns the list of TimeStamps
                             , getTimeStamps :: [BeatBar]
                             } deriving Functor

-- | Clustering 'ProbChord's in a collection of chords that share a key
data ProbChordSeg = Segment { segKey    :: Key 
                            , segChords :: [TimedData [ProbChord]] }
  
-- | Combines a 'ChordLabel' with a probability.
data ProbChord = ProbChord {chordLab :: ChordLabel, prob :: NumData}

-- | A chord candidate: an intermediate datatype that matches shorthand, 
-- chord structure and root note (plus inversion)
data ChordCand = ChordCand { originalRootCC   :: Root
                           , inversionRootCC  :: Root
                           , shorthardCC      :: Shorthand
                           , chordStructCC    :: ChordStruct }
  deriving Show
  
type ChordStruct = [NumData] 

-- | For now, we fix the number of available beats to four, because this is also
-- hard-coded into the bar and beat-tracker.
data Beat = One | Two | Three | Four | NoBeat deriving (Eq, Enum)

-- | An iterable list of Roots
chromaPC ::[Root]  
chromaPC = [ Note Nothing   C
           , Note (Just Fl) D
           , Note Nothing   D
           , Note (Just Fl) E
           , Note Nothing   E
           , Note Nothing   F
           , Note (Just Sh) F
           , Note Nothing   G
           , Note (Just Fl) A
           , Note Nothing   A
           , Note (Just Fl) B
           , Note Nothing   B
           ]


--------------------------------------------------------------------------------
-- Instances of high-level data structure
-------------------------------------------------------------------------------- 

instance Eq (ProbChord) where
  a == b = chordLab a == chordLab b

-- TODO remove line-endings from show instances

instance Show (ProbChord) where 
  show (ProbChord (Chord r sh _ _ _) _p) = 
    show r ++ ':' : show sh -- ++ ':' : printf "%.2f" p  

instance Show a => Show (TimedData a) where 
  show td = (show . getData $ td) ++ " (" ++ (show . onset  $ td) 
                                  ++ ':'   : (show . offset $ td) ++ ")\n"

instance Show ProbChordSeg where
  show pc = concatMap (\x -> show (segKey pc) ++ ' ' : show x) (segChords pc)

instance Show Beat where
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show NoBeat = "x"

instance Show BeatBar where
  show (BeatBar t bt) = '(' : show t ++ ", " ++ show bt ++ ")"
  show (Time t)       = '(' : show t ++ ")"
  
-- instance Show a => Show (BeatTimedData a) where
  -- show (BeatTimedData dat bt on off) = 
    -- show bt ++ ';' : show dat ++ ';' : show on ++ ';' : show off ++ "\n"
    
--------------------------------------------------------------------------------
-- numerical data representation
--------------------------------------------------------------------------------

-- | Groups the three types of VAMP plug-in data: 'ChordinoData', 
-- 'BeatBarTrackData', and 'KeyStrengthData'. See for more information:
--
-- * <http://www.vamp-plugins.org>
--
-- * <http://isophonics.net/nnls-chroma>
--
-- * <http://omras2.org/SonicAnnotator>
data AudioFeat = AudioFeat { getChroma      :: ChordinoData 
                           , getBeats       :: BeatBarTrackData
                           , getKeys        :: KeyStrengthData 
                           , getAudioFeatId :: FilePath}

type ChordinoData = [ChordinoLine]  

-- | Represents two chroma features and a time stamp. 
data ChordinoLine = ChordinoLine -- TODO remove useless type synonym
  { 
  -- | Returns the time stamp of the chroma features
    time ::  NumData 
  -- | Returns the bass chroma feature
  , bass :: [NumData]   -- each of the lists has always 12 elements 
  -- | Returns the treble chroma feature
  , treb :: [NumData]   -- A, Bb, B, C, Db, D, Eb, E, F, F#, G, Ab 
  } deriving (Eq, Show) -- and is shifted 3 positions to match C, Db, .., B
  
type KeyStrengthData = ChordinoData  

type BeatTrackerData = [NumData]

-- TODO Rename to BeatTime
-- | Combines a 'Beat' and a timestamp
data BeatBar = BeatBar NumData Beat
             | Time    NumData      deriving Eq

type BeatBarTrackData = [BeatBar]

type BeatChroma = TimedData [ChordinoLine] -- one list per beat

-- we compare based on the timestamp only
instance Ord BeatBar where
  compare a b = compare (timeStamp a) (timeStamp b)

--------------------------------------------------------------------------------
-- Some type conversion utilities
--------------------------------------------------------------------------------

concatTimedData :: a -> TimedData a -> TimedData a -> [TimedData a]
concatTimedData dat (TimedData _ ta) (TimedData _ tb) = 
  [ TimedData dat (mergeBeatTime ta tb) ]

mergeBeatTime :: [BeatBar] -> [BeatBar] -> [BeatBar]
mergeBeatTime [] b = b
mergeBeatTime a [] = a
mergeBeatTime a b = case compare (timeStamp . last $ a) (timeStamp. head $ b) of
  GT -> error "HarmTrace.Base.MusicTime.mergeBeatTime: cannot merge BeatTimes"
  EQ -> a ++ tail b -- do not include the same timestamp twice
  LT -> a ++ b
  
-- | Converts  'BeatBarTrackData' into 'BeatTrackerData'
getBeatTrack :: BeatBarTrackData -> BeatTrackerData
getBeatTrack = map timeStamp

-- | wraps a datatype in 'TimedData'
setData :: TimedData a -> b -> TimedData b
setData td d = td {getData = d}

-- | Returns the start time stamp
getBeatBar :: TimedData a -> BeatBar
getBeatBar td = case getTimeStamps td of
  []    -> error "HarmTrace.Base.MusicTime.getOnset: no timestamps are stored"
  (h:_) -> h

-- | Returns the start 'Beat'
getBeat :: TimedData a -> Beat
getBeat = beat . getBeatBar 
  
-- | Returns the onset time stamp
onset :: TimedData a -> NumData
onset = timeStamp . getBeatBar 

timeStamp :: BeatBar -> NumData
timeStamp (BeatBar t _bt) = t
timeStamp (Time    t    ) = t  

beat :: BeatBar -> Beat
beat (BeatBar _t bt) = bt
beat (Time    _t   ) = NoBeat

-- setOnset :: TimedData a -> NumData -> TimedData a
-- setOnset td on = td {onset = on}

-- | Returns the offset time stamp
offset :: TimedData a -> NumData
offset td = case getTimeStamps td of
  []  -> error "HarmTrace.Base.MusicTime.getOffset: no timestamps are stored"
  l   -> timeStamp . last $ l

-- setOffset :: TimedData a -> NumData -> TimedData a
-- setOffset td off = td {offset = off}

-- | Adds 'Beat' information to a 'Timed' datatype
-- setBeat :: TimedData a -> Beat -> TimedData a
-- setBeat td bt = td {getBeat = bt}

nextBeat, prevBeat :: Beat -> Beat 

-- TODO: replace by ad-hoc enum instance
-- | returns the next beat, e.g. @ nextBeat Two = Three @. 
-- Following the (current) definition of 'Beat', we still assume 4/4, in the 
-- future this function should also have the meter as an argument.
nextBeat Four = One
nextBeat b    = succ b

-- | returns the previous 'Beat', similar to 'prevBeat'.
prevBeat One  = Four
prevBeat b    = pred b

-- | Updates transforms ChordLabel wrapped in a 'ProbChord' and 'TimedData'
updateTPChord :: (ChordLabel -> ChordLabel) -> TimedData ProbChord 
              -> TimedData ProbChord
updateTPChord f = fmap (update f) where
  update g (ProbChord c p) = (ProbChord (g c) p)

-- | drops the probabilties paired with chordlabels (in a list of 'ProbChord's)
-- and returns a list of 'ChordLabel's
dropProb :: [TimedData ProbChord] -> [TimedData ChordLabel]
dropProb = map (fmap chordLab)

-- | drops the time (with or without 'Beat') information of a list 
-- 'Timed' data structure 
dropTimed :: [TimedData a] -> [a]
dropTimed = map getData


-- -- | Returns the time stamp of a 'BeatBar'
-- timeStamp :: BeatBar -> NumData
-- timeStamp = fst . beatBar

-- -- | Returns the 'Beat' of a 'BeatBar'
-- beat :: BeatBar -> Beat
-- beat = snd . beatBar

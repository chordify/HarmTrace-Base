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
    Timed (..)
  
  -- * Datatypes
  -- ** Types for representing Chords and their probabilities
  , ChordBeatAnnotation 
  , ChordAnnotation 
  , ProbChordSeg (..)
  , ProbChord (..)
  , ChordCand (..)
  
  -- ** Representing musical time
  , TimedData (..)
  , BeatTimedData (..)
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
  -- ** Type conversion and other utilities
  , getBeatTrack
  , getBeat 
  , setBeat 
  , nextBeat
  , prevBeat 
  , dumpBeats
  , dumpBeat
  , dropTimed
  , timeStamp
  , beat 

  -- ** miscellaneous
  , chromaPC
)where
             
import HarmTrace.Base.MusicRep
import Text.Printf (printf)
-- import Control.DeepSeq


-- | A type synonym is defined for our main numerical representation, this 
-- allows us to easily change the precision.
type NumData = Double

--------------------------------------------------------------------------------
-- High-level structure
--------------------------------------------------------------------------------

-- | Represents a chord transcription, similar to 'ChordAnnotation', but 
-- 'ChordBeatAnnotation' also contains 'Beat' information.
type ChordBeatAnnotation = [BeatTimedData ChordLabel]

-- | A chord annotation consists of a
-- list with chords and segment boundaries.
type ChordAnnotation = [TimedData ChordLabel]


-- TODO: * combine TimedData and BeatTimedData into one datatype 
--       * deriving Functor / Applicative
--       * remove Timed/ Functor instances / class -> make into records
data TimedData a = TimedData a NumData NumData deriving Functor

-- | A datatype that wraps around an arbitrary datatype, adding (in this order)
-- a 'Beat', an onset, and an offset.
data BeatTimedData a = BeatTimedData a Beat NumData NumData deriving Functor

-- | An alternative constructor for a BeatTimedData using two BeatBar datatypes
-- instead of a 'Beat' and two 'NumData's.
-- beatTimedData :: a -> BeatBar -> BeatBar -> BeatTimedData a
-- beatTimedData a on off = let (onnum,onbt) = beatBar on 
                         -- in  BeatTimedData a onbt onnum (fst $ beatBar off)

-- | Clustering 'ProbChord's in a collection of chords that share a key
data ProbChordSeg = Segment { segKey    :: Key 
                            , segChords :: [BeatTimedData [ProbChord]] }
  
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
data Beat = One | Two | Three | Four deriving (Eq, Enum)

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

-- | 'Timed' provides an interface for datatypes that add (musical) time 
-- information to other datatypes. Hence, it allows for accessing the fields
-- of 'TimedData' and 'BeatTimedData' via the same interface.
class Timed t where
  -- | Returns the contained datatype 
  getData   :: t a -> a
  -- | Returns the onset time stamp
  onset     :: t a -> NumData
  -- | Returns the offset time stamp
  offset    :: t a -> NumData
  -- | wraps a datatype in 't'
  setData   :: t a -> b       -> t b
  -- | Sets the onset time stamp
  setOnset  :: t a -> NumData -> t a
  -- | Sets the offset time stamp
  setOffset :: t a -> NumData -> t a
           
instance Timed TimedData where
  getData   (TimedData d _  _  ) = d
  onset     (TimedData _ on _  ) = on
  offset    (TimedData _ _  off) = off
  setData   (TimedData _ on off) d   = TimedData d on off
  setOnset  (TimedData d _  off) on  = TimedData d on off
  setOffset (TimedData d on _  ) off = TimedData d on off
  
instance Timed BeatTimedData where
  getData   (BeatTimedData d _ _  _  ) = d
  onset     (BeatTimedData _ _ on _  ) = on
  offset    (BeatTimedData _ _ _  off) = off
  setData   (BeatTimedData _ b on off) d   = BeatTimedData d b on off
  setOnset  (BeatTimedData d b _  off) on  = BeatTimedData d b on off
  setOffset (BeatTimedData d b on _  ) off = BeatTimedData d b on off

--------------------------------------------------------------------------------
-- NFData instances
-------------------------------------------------------------------------------- 

-- -- Simplified
-- instance NFData (TimedData ChordLabel) where
  -- rnf (TimedData a b c) = a `seq` rnf b `seq` rnf c

-- instance NFData Beat where  
  -- rnf One   = ()
  -- rnf Two   = ()
  -- rnf Three = ()
  -- rnf Four  = ()

--------------------------------------------------------------------------------
-- Instances of high-level data structure
-------------------------------------------------------------------------------- 

instance Eq (ProbChord) where
  a == b = chordLab a == chordLab b

-- TODO remove line-endings from show instances

instance Show (ProbChord) where 
  show (ProbChord (Chord r sh _ _ _) p) = 
    show r ++ ':' : show sh ++ ':' : printf "%.2f" p  

instance Show a => Show (TimedData a) where 
  show (TimedData bk s l) = show bk ++ " (" ++ show s ++ ':' : show l ++ ")\n"

instance Show ProbChordSeg where
  show pc = concatMap (\x -> show (segKey pc) ++ ' ' : show x) (segChords pc)

instance Show Beat where
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"

instance Show BeatBar where
  show = show . beatBar
  
instance Show a => Show (BeatTimedData a) where
  show (BeatTimedData dat bt on off) = 
    show bt ++ ';' : show dat ++ ';' : show on ++ ';' : show off ++ "\n"
    
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

newtype BeatBar = BeatBar {beatBar :: (NumData, Beat)} deriving Eq

type BeatBarTrackData = [BeatBar]

type BeatChroma = BeatTimedData [ChordinoLine] -- one list per beat

-- we compare based on the timestamp only
instance Ord BeatBar where
  compare (BeatBar (b1,_)) (BeatBar (b2,_)) = compare b1 b2

--------------------------------------------------------------------------------
-- Some type conversion utilities
--------------------------------------------------------------------------------

-- | Converts  'BeatBarTrackData' into 'BeatTrackerData'
getBeatTrack :: BeatBarTrackData -> BeatTrackerData
getBeatTrack = map (fst . beatBar)

-- | Provides access to the 'Beat' field of a 'BeatTimedData'. The other fields
-- should be accessed by the methods of the 'Timed' class.
getBeat :: BeatTimedData a -> Beat
getBeat (BeatTimedData _ b _ _) = b

-- | Adds 'Beat' information to a 'Timed' datatype
setBeat :: Timed t => t a -> Beat -> BeatTimedData a
setBeat tdat bt = BeatTimedData (getData tdat) bt (onset tdat) (offset tdat)

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

-- | drops the time (with or without 'Beat') information of a list 
-- 'Timed' data structure 
dropTimed :: Timed t => [t a] -> [a]
dropTimed = map getData

-- | Converts a list of 'BeatTimedData's into a list of 'TimedData's
dumpBeats :: [BeatTimedData a] -> [TimedData a]
dumpBeats = map dumpBeat

-- | Converts a 'BeatTimedData' into a 'TimedData'
dumpBeat :: BeatTimedData a -> TimedData a
dumpBeat (BeatTimedData dat _bt on off) = TimedData dat on off

-- | Returns the time stamp of a 'BeatBar'
timeStamp :: BeatBar -> NumData
timeStamp = fst . beatBar

-- | Returns the 'Beat' of a 'BeatBar'
beat :: BeatBar -> Beat
beat = snd . beatBar

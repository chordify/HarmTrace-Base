{-# OPTIONS_GHC -Wall             #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveGeneric        #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Chord.PitchClass
-- Copyright   :  (c) 2012--2016, Chordify BV
-- License     :  LGPL-3
--
-- Maintainer  :  haskelldevelopers@chordify.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: this module provides some functions that transform notes and chords
-- into pitch classes and pitch class sets. See for more info:
-- <http://en.wikipedia.org/wiki/Pitch_class>
--------------------------------------------------------------------------------
module HarmTrace.Base.Chord.PitchClass (
    PCSet  -- Pitch Class Set
  , pc     -- Unwraps a 'PCSet'
    -- * Pitch classes
  , toPitchClass
  , pcToRoot
    -- * Pitch classes applied to chords
  , toPitchClasses
  , rootPC
  , bassPC
  , ignorePitchSpelling
  , altPitchSpelling
    -- * Pitch classes applied to keys
  , keyPitchClasses
    -- * Pitch classes applied to interval sets
  , intValToPitchClss
  , intSetToPC
  -- * Enharmonic Equivalence
  , EnHarEq (..)
  -- * Diatonic Class
  , Diatonic
  ) where

import HarmTrace.Base.Chord.Datatypes
import HarmTrace.Base.Chord.Intervals
import HarmTrace.Base.Chord.Internal

import Data.Binary                ( Binary )
import Data.IntSet                ( IntSet, fromList, union )
import qualified Data.IntSet as S ( map )
import GHC.Generics               ( Generic )

-- | We hide the constructors, such that a PCSet can only be constructed with
-- 'toPitchClasses', this to overcome confusion between interval sets and
-- pitch class sets, which are both 'Data.IntSet.IntSet's
newtype PCSet = PCSet {pc :: IntSet} deriving (Show, Eq, Generic)

instance Binary PCSet

-- | The (relative) notes in a major scale
majorScale :: Num a => [a]
majorScale = [0,2,4,5,7,9,11]

-- | The (relative) notes in a minor scale
minorScale :: Num a => [a]
minorScale = [0,2,3,5,7,8,10]

-- | Returns the semitone value [0 .. 11] of a 'ScaleDegree' where
-- 0 = C, e.g. F# = 6. For the constructors 'N' and 'X' an error is thrown.
toPitchClass :: (Diatonic a) => Note a -> Int
toPitchClass (Note m p)
  | ix <= 6   = ((majorScale !! ix) + modToInt m) `mod` 12
  | otherwise = error ("HarmTrace.Base.MusicRep.toPitchClass: no semitone for "
                        ++ show p ++ show m )
      where ix = fromEnum p

-- | Transforms an interval set to and a root into a 'PCSet'
intSetToPC :: IntSet -> Root -> PCSet
intSetToPC is r = PCSet . S.map (transp (toPitchClass r)) $ is where

  transp :: Int -> Int -> Int
  transp t i = (i + t) `mod` 12


-- | As 'toIntervalClss', but returns the 'Int' pitch class.
intValToPitchClss :: Root -> Interval -> Int
intValToPitchClss r i = (toPitchClass r + toIntervalClss i) `mod` 12


-- | The reverse of 'toPitchClass' returning the 'Note DiatonicNatural' given a
-- Integer [0..11] semitone, where 0 represents C. All pitch spelling is ignored
-- and the the following twelve pitch names will be output: C, C#, D, Eb, E, F
-- F#, G, Ab, A, Bb, B.  When the integer is out of the range [0..11] an
-- error is thrown.
pcToRoot :: Int -> Root
pcToRoot i
  | 0 <= i && i <= 11 = roots !! i
  | otherwise         = error ("HarmTrace.Base.MusicRep.toRoot " ++
                               "invalid pitch class: " ++ show i)

-- | Similar to 'toIntSet' but returns 'Int' pitch classes and includes the
-- 'Root' and the bass 'Note' of the the 'Chord'. 'toPitchClasses' throws an
-- error when applied to a 'NoChord' or 'UndefChord'.
toPitchClasses :: ChordLabel -> PCSet
toPitchClasses c = catchNoChord "Chord.PitchClass.toPitchClasses"
                                (intSetToPC ivs . chordRoot) c

  where ivs = toIntSet c `union` fromList [0, toIntervalClss (chordBass c)]

-- | Return the set of pitches for the given key.
keyPitchClasses :: Key -> PCSet
keyPitchClasses k = intSetToPC (fromList scale) (keyRoot k) where
  scale = case keyMode k of
    MajMode -> majorScale
    MinMode -> minorScale

-- | A short-cut applying 'intValToPitchClss' to a 'Chord'. 'bassPC' throws an
-- error when applied to a 'NoChord' or 'UndefChord'.
bassPC :: ChordLabel -> Int
bassPC = catchNoChord "Chord.PitchClass.rootPC" bassPC' where

  bassPC' :: ChordLabel -> Int
  bassPC' c = intValToPitchClss (chordRoot c) (chordBass c)

-- | A short-cut applying 'toPitchClass' to a 'Chord'. 'rootPC'  throws an
-- error when applied to a 'NoChord' or 'UndefChord'.
rootPC :: ChordLabel -> Int
rootPC = catchNoChord "Chord.PitchClass.rootPC" (toPitchClass . chordRoot)

-- | Ignores the pitch spelling of a chord by applying 'pcToRoot' and
-- 'toPitchClass' to the root of a 'ChordLabel'.
ignorePitchSpelling :: ChordLabel -> ChordLabel
ignorePitchSpelling NoChord    = NoChord
ignorePitchSpelling UndefChord = UndefChord
ignorePitchSpelling c          = fmap (pcToRoot . toPitchClass) c

-- | Give the alternative pitch spelling of a chord (if it exists)
altPitchSpelling :: ChordLabel -> Maybe ChordLabel
altPitchSpelling NoChord    = Nothing
altPitchSpelling UndefChord = Nothing
altPitchSpelling (Chord (Note acc root) short add intervc) = case acc of
  Nat -> Nothing
  FF  -> Nothing -- todo: is this right?
  SS  -> Nothing -- todo: is this right?
  Fl  -> Just $ Chord (Note Sh (pred root)) short add intervc
  Sh  -> Just $ Chord (Note Fl (succ root)) short add intervc

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

-- | A class to compare datatypes that sound the same (they contain the
-- same pitch class content):
-- <http://en.wikipedia.org/wiki/Enharmonic>
class EnHarEq a where
  (&==) :: a -> a -> Bool
  (&/=) :: a -> a -> Bool

  a &== b = not (a &/= b)
  a &/= b = not (a &== b)

instance Diatonic a => EnHarEq (Note a) where
  a &== b = toPitchClass a == toPitchClass b

instance EnHarEq ChordLabel where
  a &== b = toPitchClasses a == toPitchClasses b

-- | A class to mark certain datatypes to have a diatonic structure:
--   http://en.wikipedia.org/wiki/Diatonic_and_chromatic
class (Generic a, Show a, Enum a, Bounded a) => Diatonic a

instance Diatonic DiatonicNatural
instance Diatonic DiatonicDegree

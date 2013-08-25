{-# OPTIONS_GHC -Wall             #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.MusicRep
-- Copyright   :  (c) 2013 W. Bas de Haas and Jose Pedro Magalhaes
-- License     :  LGPL-3
--
-- Maintainer  :  bas@chordify.net, dreixel@chordify.net 
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: 
--------------------------------------------------------------------------------

module HarmTrace.Base.Chord.Analysis (
  -- * Analysis
  -- ** Triads and Sevenths
    analyseTriad
  , analyseTetra
  , toTriad
  , toMajMinChord
    -- ** Misc
  , toMode
  , toMajMin
  , toClassType

  -- * Interval classes
  , toIntervalClss  
  , intervalToPitch
  , toChord
  -- * Scale degree transposition
  , transposeRoot
  , transposeSD
  , toChordDegree
  , toScaleDegree
  , icToInterval
  -- ** Classes
  , Diatonic
  ) where
  
import HarmTrace.Base.Chord.Datatypes
import HarmTrace.Base.Chord.PitchClass
import HarmTrace.Base.Chord.Intervals
import HarmTrace.Base.Chord.Internal

import Data.IntSet                ( IntSet, toAscList, member, (\\) )
  
--------------------------------------------------------------------------------
-- Transformation and analysis of chords
--------------------------------------------------------------------------------

-- | Returns the 'ClassType' given a 'Chord'. This function uses 
-- 'analyseDegClassType' to analyse a chord and derive the 'ClassType'
toClassType :: Chord a -> ClassType
toClassType (Chord  _r  sh []   _b) = shToClassType sh -- no additions
-- combine the degrees and analyse them. N.B., also NoAdd degrees are resolved
toClassType c = analyseDegClassType . toIntSet $ c

-- | Analyses a degree list and returns 'MajTriad', 'MinTriad' or 'NoTriad' if
-- the degrees make a chord a major, minor, or no triad, respectively.
analyseDegClassType :: IntSet -> ClassType
analyseDegClassType degs = 
    case (analyseThird degs, analyseFifth degs, analyseSevth degs) of
       -- Triads
       (MinThird, DimFifth , DimSev) -> DimClass
       (MajThird, _        , MinSev) -> DomClass
       (_       , AugFifth , _     ) -> DomClass
       (MajThird, DimFifth , _     ) -> DomClass
       (MajThird, _        , _     ) -> MajClass
       (MinThird, PerfFifth, _     ) -> MinClass
       (MinThird, _        , _     ) -> MinClass
       (NoThird,  _        , _     ) -> NoClass


       
       
-- | Categorises a 'Shorthand' into a 'ClassType'.
shToClassType :: Shorthand -> ClassType
shToClassType Maj     = MajClass
shToClassType Min     = MinClass
shToClassType Dim     = DimClass
shToClassType Aug     = DomClass
shToClassType Maj7    = MajClass
shToClassType Min7    = MinClass
shToClassType Sev     = DomClass
shToClassType Dim7    = DimClass
shToClassType HDim7   = MinClass
shToClassType MinMaj7 = MinClass
shToClassType Aug7    = DomClass
shToClassType Maj6    = MajClass 
shToClassType Min6    = MinClass
shToClassType Nin     = DomClass
shToClassType Maj9    = MajClass
shToClassType Min9    = MinClass
shToClassType Five    = NoClass
shToClassType Sus2    = NoClass
shToClassType Sus4    = NoClass
shToClassType SevSus4 = NoClass
shToClassType None    = NoClass
-- additional Billboard shorthands
shToClassType Min11    = MinClass
shToClassType Eleven   = DomClass
shToClassType Min13    = MinClass
shToClassType Maj13    = MajClass
shToClassType Thirteen = DomClass

-- should not be exported, used only in toTriad
data Third = MajThird | MinThird             | NoThird deriving (Eq, Show)
data Fifth = DimFifth | PerfFifth | AugFifth | NoFifth deriving (Eq, Show)
data Sevth = DimSev   | MinSev    | MajSev   | NoSev   deriving (Eq, Show)

triadToSh :: Triad -> Shorthand
triadToSh t = case t of
                 MajTriad -> Maj 
                 MinTriad -> Min 
                 AugTriad -> Aug 
                 DimTriad -> Dim 
                 NoTriad  -> None

analyseTetra :: IntSet -> Shorthand
analyseTetra is = case (analyseTriad is, analyseSevth is) of
                    (MajTriad, MinSev) -> Sev
                    (MajTriad, MajSev) -> Maj7
                    (MinTriad, MinSev) -> Min7
                    (MinTriad, MajSev) -> MinMaj7
                    (DimTriad, MinSev) -> HDim7
                    (DimTriad, DimSev) -> Dim7
                    (AugTriad, MinSev) -> Aug7
                    (t       , NoSev ) -> triadToSh t
                    _                  -> None                 

-- | Takes a 'Chord' and determines the 'Triad'
--
-- >>> toTriad (Chord (Note Nat C) Min [NoAdd (Note Fl I3),Add (Note Nat I3)] 0 0)
-- maj 
--
-- >>> toTriad (Chord (Note Nat C) HDim7 [Add (Note Sh I11)] 0 0)
-- dim
--
-- >>> toTriad (Chord (Note Nat C) Min [NoAdd (Note Fl I3)] 0 0)
-- NoTriad
--
toTriad :: Chord a -> Triad
toTriad (Chord  _r  sh [] _b) = shToTriad sh -- there are no additions
-- combine the degrees and analyse them. N.B., also NoAdd degrees are resolved
toTriad c = analyseTriad . toIntSet $ c
   
-- | Analyses a degree list and returns 'MajTriad', 'MinTriad' or 'NoTriad' if
-- the degrees make a chord a major, minor, or no triad, respectively.
analyseTriad :: IntSet -> Triad
analyseTriad is =  
    case (analyseThird is, analyseFifth is) of
       (MajThird, PerfFifth) -> MajTriad
       (MajThird, AugFifth ) -> AugTriad
       (MajThird, DimFifth ) -> NoTriad
       (MinThird, PerfFifth) -> MinTriad
       (MinThird, AugFifth ) -> NoTriad
       (MinThird, DimFifth ) -> DimTriad
       (NoThird,  _        ) -> NoTriad
       (_      ,  NoFifth  ) -> NoTriad
      
-- analyses the third in a degree list
analyseThird :: IntSet -> Third
analyseThird is
  | member 4 is = MajThird
  | member 3 is = MinThird
  | otherwise   = NoThird
      
-- analyses the fifth in a degree list 
analyseFifth :: IntSet -> Fifth
analyseFifth is 
  | member 7 is = PerfFifth
  | member 6 is = DimFifth
  | member 8 is = AugFifth
  | otherwise   = NoFifth

-- analyses the fifth in a degree list 
analyseSevth :: IntSet -> Sevth
analyseSevth is
  | member 10 is = MinSev
  | member 11 is = MajSev
  | member 9  is = DimSev
  | otherwise    = NoSev

 
-- | Converts a 'Shorthand' to a 'Triad' 
-- N.B. this function should not be exported because the shorthand alone cannot
-- determine the triad 
shToTriad :: Shorthand -> Triad     
shToTriad Maj     = MajTriad
shToTriad Min     = MinTriad
shToTriad Dim     = DimTriad
shToTriad Aug     = AugTriad
shToTriad Maj7    = MajTriad
shToTriad Min7    = MinTriad
shToTriad Sev     = MajTriad
shToTriad Dim7    = DimTriad
shToTriad HDim7   = DimTriad
shToTriad MinMaj7 = MinTriad
shToTriad Aug7    = AugTriad
shToTriad Maj6    = MajTriad 
shToTriad Min6    = MinTriad
shToTriad Nin     = MajTriad
shToTriad Maj9    = MajTriad
shToTriad Min9    = MinTriad
shToTriad Five    = NoTriad
shToTriad Sus2    = NoTriad
shToTriad Sus4    = NoTriad
shToTriad SevSus4 = NoTriad
shToTriad None    = NoTriad
-- additional Billboard shorthands
shToTriad Min11    = MinTriad
shToTriad Eleven   = MajTriad
shToTriad Min13    = MinTriad
shToTriad Maj13    = MajTriad
shToTriad Thirteen = MajTriad



      
-- | Converts a 'Shorthand' to a 'Mode'
toMode :: Triad -> Mode     
toMode MajTriad = MajMode
toMode MinTriad = MinMode
toMode t        = error (  "HarmTrace.Base.MusicRep.toMode: cannot convert "
                        ++ " triad to mode: " ++ show t)

-- | Converts a 'Shorthand' to either a 'MajClass', 'MinClass' or 'NoClass' 
-- 'ClassType'.
toMajMin :: Triad -> ClassType
toMajMin MajTriad = MajClass
toMajMin MinTriad = MinClass
toMajMin AugTriad = MajClass
toMajMin DimTriad = MinClass
toMajMin NoTriad  = NoClass

-- | applies 'toMajMin' to a 'Chord'
toMajMinChord :: ChordLabel -> ChordLabel
toMajMinChord c = c {chordShorthand = majMinSh, chordAdditions = []}
  where majMinSh = case toMajMin (toTriad c) of
                     MajClass -> Maj
                     MinClass -> Min
                     NoClass  -> None
                     -- catch all: cannot happen, see toMajMin
                     _        -> error ("HarmTrace.Base.MusicRep.toMajMinChord"
                                        ++ " unexpected chord " ++ show c)
--------------------------------------------------------------------------------
-- Value Level Scale Degree Transposition
-------------------------------------------------------------------------------- 
    
-- Chord root shorthand degrees location duration
-- | Given a 'Key', calculates the the 'ChordDegree' (i.e. relative, 
-- 'ScaleDegree' based 'Chord') for an absolute 'ChordLabel' using 
-- 'toScaleDegree'.
toChordDegree :: Key -> ChordLabel -> ChordDegree
toChordDegree k (Chord r sh a b) = Chord (toScaleDegree k r) sh a b
toChordDegree _ c = 
  error("HarmTrace.Base.MusicRep: cannot create scale degree for " ++ show c)
    
-- | Transformes a absolute 'Root' 'Note' into a relative 'ScaleDegree', given
-- a 'Key'.
toScaleDegree :: Key -> Root -> ScaleDegree
-- toScaleDegree _ n@(Note _ N) = 
  -- error ("HarmTrace.Base.MusicRep.toScaleDegree: cannot transpose " ++ show n)
toScaleDegree (Key kr _) cr  = -- Note Nat I
  scaleDegrees!!(((toPitchClass cr) - (toPitchClass kr)) `mod` 12)

-- | Transposes a Root with a 'Int' semitones up
transposeRoot :: Root -> Int -> Root
transposeRoot deg sem = transpose roots deg sem 
  
-- | Transposes a scale degree with 'Int' semitones up
transposeSD :: ScaleDegree -> Int -> ScaleDegree
transposeSD deg sem = transpose scaleDegrees deg sem 

transpose :: Diatonic a => [Note a] -> Note a -> Int -> Note a
transpose ns n sem = ns !! ((sem + (toPitchClass n)) `mod` 12)



-- | Similar to 'toScaleDegree', an interval is transformed into an absolute
-- 'Root' pitch, given another 'Root' that serves as a basis. 
--  
-- >>> intervalToPitch (Note Sh G) (Note Fl I13)
-- >>> E
--  
-- >>> intervalToPitch (Note Nat C) (Note Sh I11)
-- >>> F#
--
intervalToPitch :: Root -> Interval -> Root
intervalToPitch r = pcToRoot . intValToPitchClss r

-- | Given an 'IntSet' (Interval Set), a 'Root' 'Note' and an optional 
-- bass 'Interval', returns a 'Chord' 
toChord :: Root -> IntSet -> Maybe Interval -> Chord Root
toChord r is mi = Chord r sh add (maybe (Note Nat I1) id mi)
 
 where add = map (Add . icToInterval) $ toAscList (is \\ shToIntSet sh)
       sh  = analyseTetra is     


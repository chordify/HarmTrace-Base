{-# OPTIONS_GHC -Wall             #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveGeneric        #-}

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
  -- ** Sets
    PCSet  -- ^ Pitch Class Set
  , pc     -- ^ Unwraps a 'PCSet'
  , IntSet -- ^ Interval Set
  
  -- ** Intervals
  , toIntSet
  , addToIntSet
  -- ** Triads and Sevenths
  , analyseTriad
  , analyseTetra
  , toTriad
  , toMajMinChord
    -- ** Misc
  , toMode
  , toMajMin
  , toClassType
  -- * Pitch classes
  , toPitchClass
  , toPitchClasses
  , intValToPitchClss
  , intSetToPC
  , pcToRoot
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
  , EnHarEq (..)
  ) where
  
import HarmTrace.Base.Chord.Datatypes
import Data.Maybe                 ( fromJust )
import Data.List                  ( elemIndex, intercalate, partition )
import Data.Binary                ( Binary )
import Data.IntSet                ( IntSet, fromList, union, insert, singleton
                                  , empty, toAscList, member, (\\) )
import qualified Data.IntSet as S ( map )
import GHC.Generics               ( Generic )
  
  
-- | We hide the constructors, such that a PCSet can only be constructed with
-- 'toPitchClasses', this to overcome confusion between interval sets and
-- pitch class sets, which are both 'IntSet's
newtype PCSet = PCSet {pc :: IntSet} deriving (Show, Eq)
    
  
--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class (Generic a, Show a, Enum a, Bounded a) => Diatonic a

instance Diatonic DiatonicNatural
instance Diatonic DiatonicDegree

class EnHarEq a where
  (&==) :: a -> a -> Bool
  (&/=) :: a -> a -> Bool

  a &== b = not (a &/= b)
  a &/= b = not (a &== b)

instance Diatonic a => EnHarEq (Note a) where
  a &== b = toPitchClass a == toPitchClass b
  
instance EnHarEq ChordLabel where
  a &== b = toPitchClasses a == toPitchClasses b  
  
  
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


-- | Similar to 'toIntValList' but returns 'Int' pitch classes and includes the
-- 'Root' note of the the 'Chord'.
toPitchClasses :: ChordLabel -> PCSet
toPitchClasses c = intSetToPC ivs . chordRoot $ c
  where ivs = toIntSet c `union` fromList [0, toIntervalClss (chordBass c)]

-- | Transforms a Chord into a list of relative 'Interval's (i.e. 'Addition's,
-- without the root note).
-- 
-- >>> toIntValList (Chord (Note Nat C) HDim7 [Add (Note Sh I11)] 0 0)
-- [3b,5b,7b,11#]
--
-- >>> toIntValList (Chord (Note Nat C) Min13 [NoAdd (Note Nat I11)] 0 0)
-- [3b,5,7b,9,13]
--
-- >>> toIntValList (parseData pChord "D:7(b9)")
-- [3,5,7b,9b]
--
toIntSet :: Chord a -> IntSet
toIntSet (Chord  _r sh [] _b) = shToIntSet sh
toIntSet (Chord  _r sh a  _b) = shToIntSet sh `union` addToIntSet a
toIntSet _ = error ("HarmTrace.Base.MusicRep.toIntValList: cannot create" ++
                        "interval list for N or X")

-- | Converts a list of addition to an 'IntSet' containing the relative 
-- structure of the ('Addition' list of the) 'Chord'
addToIntSet :: [Addition] -> IntSet 
addToIntSet add = toSet adds \\ toSet remv

  where (adds, remv) = partition isAddition add

        toSet :: [Addition] -> IntSet
        toSet = fromList . map (toIntervalClss . getInt)
        
        getInt :: Addition -> Interval
        getInt (NoAdd i) = i
        getInt (Add   i) = i
        
  
-- | Expands a 'Shorthand' to its list of degrees
shToIntSet :: Shorthand -> IntSet 
shToIntSet Maj     = fromList [4,7]              --    [Note Nat I3,Note Nat I5]
shToIntSet Min     = fromList [3,7]              --    [Note Fl  I3,Note Nat I5]
shToIntSet Dim     = fromList [3,6]              --    [Note Fl  I3,Note Fl  I5]
shToIntSet Aug     = fromList [4,8]              --    [Note Nat I3,Note Sh  I5]
shToIntSet Maj7    = insert 11 (shToIntSet Maj)  -- ++ [Note Nat I7]
shToIntSet Min7    = insert 10 (shToIntSet Min)  -- ++ [Note Fl  I7]
shToIntSet Sev     = insert 10 (shToIntSet Maj)  -- ++ [Note Fl  I7]
shToIntSet Dim7    = insert  9 (shToIntSet Dim)  -- ++ [Note FF  I7]
shToIntSet HDim7   = insert 10 (shToIntSet Dim)  -- ++ [Note Fl  I7]
shToIntSet MinMaj7 = insert 11 (shToIntSet Min)  -- ++ [Note Nat I7]
shToIntSet Aug7    = insert 10 (shToIntSet Aug)  -- ++ [Note Fl  I7]
shToIntSet Maj6    = insert  9 (shToIntSet Maj)  -- ++ [Note Nat I6]
-- Harte uses a 6 instead of b6
shToIntSet Min6    = insert  8 (shToIntSet Min ) -- ++ [Note Fl  I6] 
shToIntSet Nin     = insert 14 (shToIntSet Sev ) -- ++ [Note Nat I9]
shToIntSet Maj9    = insert 14 (shToIntSet Maj7) -- ++ [Note Nat I9]
shToIntSet Min9    = insert 14 (shToIntSet Min7) -- ++ [Note Nat I9]
shToIntSet Five    = singleton 7                 --    [Note Nat I5]
shToIntSet Sus2    = fromList [2,7]              --    [Note Nat I2,Note Nat I5]
shToIntSet Sus4    = fromList [5,7]              --    [Note Nat I4,Note Nat I5]
shToIntSet SevSus4 = insert 10 (shToIntSet Sus4) -- ++ [Note Fl  I7]
shToIntSet None    = empty
-- additional Billboard shorthands
shToIntSet Min11   = insert 17 (shToIntSet Min9  ) -- ++ [Note Nat I11]
shToIntSet Eleven  = insert 17 (shToIntSet Nin   ) -- ++ [Note Nat I11]
shToIntSet Min13   = insert 21 (shToIntSet Min11 ) -- ++ [Note Nat I13]
shToIntSet Maj13   = insert 21 (shToIntSet Maj9  ) -- ++ [Note Nat I13]
shToIntSet Thirteen= insert 21 (shToIntSet Eleven) -- ++ [Note Nat I13]
     
      
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

-- TODO : should be renamed to 'toPitchClass'
-- | Returns the semitone value [0 .. 11] of a 'ScaleDegree' where
-- 0 = C, e.g. F# = 6. For the constructors 'N' and 'X' an error is thrown.
toPitchClass :: (Diatonic a) => Note a -> Int
toPitchClass (Note m p) 
  | ix <= 6   = noNegatives (([0,2,4,5,7,9,11] !! ix) + modToSemi m) `mod` 12
  | otherwise = error ("HarmTrace.Base.MusicRep.toPitchClass: no semitone for "
                        ++ show p ++ show m )
      where ix = fromEnum p
            noNegatives s | s < 0     = 12 + s
                          | otherwise = s

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
 
-- | As 'intervalToPitch', but returns the 'Int' pitch class. 
intValToPitchClss :: Root -> Interval -> Int
intValToPitchClss r i = (toPitchClass r + toIntervalClss i) `mod` 12
                          
-- | Similar to 'toPitchClss', this function calculates an enharmonic 
-- interval class for each 'Note Interval' in the range of [0 .. 23]
-- ( == ['Note Nat I1' .. 'Note SS I13']
toIntervalClss :: Interval -> Int
toIntervalClss n@(Note m i) =
  --         1 2 3 4 5 6 7  8  9  10 11 12 13
  let ic = ([0,2,4,5,7,9,11,12,14,16,17,19,21] !! (fromEnum i)) + modToSemi m 
  in  if ic >= 0 then ic
                 else error ("HarmTrace.Base.MusicRep.toIntervalClss: no "
                          ++ "interval class for " ++ show n)

intSetToPC :: IntSet -> Root -> PCSet
intSetToPC is r = PCSet . S.map (transp (toPitchClass r)) $ is where

  transp :: Int -> Int -> Int
  transp t i = (i + t) `mod` 12

  
toChord :: Root -> IntSet -> Maybe Interval -> Chord Root
toChord r is mi = Chord r sh add (maybe (Note Nat I1) id mi)
 
 where add = map (Add . icToInterval) $ toAscList (is \\ shToIntSet sh)
       sh  = analyseTetra is

-- | Converts an 'Int'erval class to an 'Interval'
icToInterval :: Int -> Interval
icToInterval i
  | 0 <= i && i <= 21 = intervals !! i
  | otherwise         = error ("HarmTrace.Base.MusicRep.toInterval " ++
                               "invalid pitch class: " ++ show i)

-- | The reverse of 'toPitchClass' returning the 'Note DiatonicNatural' given a 
-- Integer [0..11] semitone, where 0 represents C. When the integer is out 
-- of the range [0..11] an error is thrown.
pcToRoot :: Int -> Root
pcToRoot i 
  | 0 <= i && i <= 11 = roots !! i
  | otherwise         = error ("HarmTrace.Base.MusicRep.toRoot " ++
                               "invalid pitch class: " ++ show i)
    
-- | Transforms type-level Accidentals to semitones (Int values)
modToSemi :: Accidental -> Int
modToSemi Nat =  0
modToSemi Sh  =  1
modToSemi Fl  = -1
modToSemi SS  =  2
modToSemi FF  = -2

-- | A list of 12 'ScaleDegree's, ignoring pitch spelling.
scaleDegrees ::[ ScaleDegree ]  
scaleDegrees = [ Note  Nat I
               , Note  Sh  I
               , Note  Nat II
               , Note  Fl  III
               , Note  Nat III
               , Note  Nat IV
               , Note  Sh  IV
               , Note  Nat V
               , Note  Fl  VI
               , Note  Nat VI
               , Note  Fl  VII
               , Note  Nat VII
               ]
               

-- | A list of 12 'Note DiatonicNatural's, ignoring pitch spelling.
roots :: [ Root ]  
roots =  [ Note Nat C
         , Note Sh  C
         , Note Nat D
         , Note Fl  E
         , Note Nat E
         , Note Nat F
         , Note Sh  F
         , Note Nat G
         , Note Fl  A
         , Note Nat A
         , Note Fl  B
         , Note Nat B
         ]


intervals :: [ Interval ]
intervals = [ Note Nat I1  --  0: Prime
            , Note Fl  I2  --  1: Minor second
            , Note Nat I2  --  2: Major second
            , Note Fl  I3  --  3: Minor third
            , Note Nat I3  --  4: Major third
            , Note Nat I4  --  5: Perfect fourth
            , Note Fl  I5  --  6: Diminished fifth (augmented fourth)
            , Note Nat I5  --  7: Perfect fifth
            , Note Fl  I6  --  8: Minor sixth
            , Note Nat I6  --  9: Major sixth
            , Note Fl  I7  -- 10: Minor seventh
            , Note Nat I7  -- 11: Major seventh
            , Note Nat I8  -- 12: Perfect Octave
            , Note Fl  I9  -- 13: Flat nine   -- in jazz jargon
            , Note Nat I9  -- 14: Nine
            , Note Sh  I9  -- 15: Sharp nine
            , Note Nat I10 -- 16: tenth: is viewed as third
            , Note Nat I11 -- 17: eleventh
            , Note Sh  I11 -- 18: sharp eleventh
            , Note Nat I12 -- 19: twelveth: viewed as fifth
            , Note Fl  I13 -- 20: Flat thirteen
            , Note Nat I13 -- 21: Thirteen
            ]
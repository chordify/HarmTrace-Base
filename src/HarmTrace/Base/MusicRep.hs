{-# OPTIONS_GHC -Wall             #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveGeneric        #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.MusicRep
-- Copyright   :  (c) 2012--2013 W. Bas de Haas and Jose Pedro Magalhaes
-- License     :  LGPL-3
--
-- Maintainer  :  bas@chordify.net, dreixel@chordify.net 
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A set of types and classes for representing musical chords. The 
-- chord datatypes are based on the unambiguous chord representation presented 
-- in: Christopher Harte, Mark Sandler and Samer Abdallah (2005), 
-- /Symbolic representation of musical chords: a proposed syntax for text annotations/, 
-- In: Proceedings of 6th International Conference on Music Information 
-- Retrieval (<http://ismir2005.ismir.net/proceedings/1080.pdf>). 
--------------------------------------------------------------------------------

module HarmTrace.Base.MusicRep (
  -- * Representing musical chords and keys
    PieceLabel (..)
  , Note (..)
  , Accidental (..)
  , Root 
  , DiatonicNatural (..)
  , ScaleDegree 
  , DiatonicDegree (..)
  , Diatonic
  -- ** Keys
  , Key (..)
  , Mode (..)
  -- ** Chords
  , Chord (..)
  -- , Class
  , Shorthand (..)
  , Addition (..)
  , IntNat (..)
  , Interval 
  , ChordLabel
  , ChordDegree
  -- * Derived types for classification of chords
  , ClassType (..)
  , Triad (..)
  -- * Tests
  , isNoneChord
  , isAddition
  -- ** Transformation
  -- * Transformation and analysis of chords
  , toClassType
  , toTriad
  , analyseDegTriad
  , toIntValList
  , addToIntValList
  , toMode
  , toMajMin
  , toMajMinChord
  -- * Scale degree transposition
  , toChordDegree
  , toScaleDegree
  , intValToPitch
  , toPitchClasses
  , transposeRoot
  , transposeSD
  , toPitchClass
  , toIntervalClss
  , toRoot
  ) where
  
import Data.Maybe            ( fromJust )
import Data.List             ( elemIndex, intercalate, (\\), partition )
import Data.Binary           ( Binary )
import Data.IntSet           ( IntSet, fromList, union, insert, singleton
                             , empty, toAscList )
import qualified Data.IntSet as S ( (\\), map )
import GHC.Generics          ( Generic )
 
--------------------------------------------------------------------------------
-- Representing musical information at the value level
--------------------------------------------------------------------------------


-- | A musical key consising of a 'Root' and 'Mode'
data Key  = Key { keyRoot :: Root, keyMode :: Mode } deriving (Eq, Ord, Generic)

-- | The 'Mode' of a key, which can be major or minor
data Mode = MajMode | MinMode deriving (Eq, Ord, Generic)

-- | A container type combinint a key and a list of 'ChordLabel's
data PieceLabel = PieceLabel Key [ChordLabel] deriving Generic

-- | A chord based on absolute 'Root' notes
type ChordLabel   = Chord Root

-- | A chord based on relative 'ScaleDegree's
type ChordDegree  = Chord ScaleDegree

-- | The representation for a single chord 
data Chord a = Chord { chordRoot        :: a
                       -- ^ the 'Root' note of a chord
                     , chordShorthand   :: Shorthand
                       -- ^ a 'Shorthand' representing the interval
                       -- structure of a chord
                     , chordAdditions   :: [Addition]
                       -- ^ the (additional) interval structure
                     , chordBass        :: Interval
                       -- ^ the sounding bass note, denoting an inversion when
                       -- it is not a 'Note Nat I1'
                     } 
              | NoChord     -- ^ No sounding chord (silence, noise, etc.)
              | UndefChord  -- ^ An undefined chord
                deriving (Eq, Generic)

-- | We introduce four chord categories: major chords, minor chords, dominant
-- seventh chords, and diminished seventh chords
data ClassType = MajClass | MinClass | DomClass | DimClass | NoClass
  deriving (Eq, Enum, Ord, Bounded, Generic)

-- Following Harte et al., we define a number of chord 'Shorthand's
data Shorthand = -- | Triadic chords
                 Maj | Min | Dim | Aug
                 -- | Seventh chords
               | Maj7 | Min7 | Sev | Dim7 | HDim7 | MinMaj7 | Aug7
                 -- | Sixth chords
               | Maj6 | Min6
                 -- | Extended chords
               | Nin | Maj9 | Min9
                 -- | Suspended chords
               | Sus4 | Sus2 | SevSus4
                 -- | Power chords
               | Five
                 -- Only a root note
               | None
                 -- Additional shorthands in billboard collection
               | Eleven | Thirteen | Min11 | Maj13 | Min13
               
  deriving (Eq, Ord, Enum, Bounded, Generic) 


-- | Key relative scale degrees to abstract from the absolute Root notes
type ScaleDegree = Note DiatonicDegree

-- | All Diatonic scale degrees 
data DiatonicDegree = I | II | III | IV | V | VI | VII 
                    | Imp -- ^ for unrepresentable scale degrees
  deriving (Show, Eq, Enum, Ord, Bounded, Generic)

-- | Representing absolute 'Root' notes  
type Root = Note DiatonicNatural

-- | The seven diatonic naturals
data DiatonicNatural =  C | D | E | F | G | A | B 
                     -- |  N -- ^ for no root
                     -- |  X -- ^ for representing unknown roots (used in MIREX)
  deriving (Show, Eq, Enum, Ord, Bounded, Generic)
  
-- | Intervals for additional chord notes    
data Addition = Add   Interval
              | NoAdd Interval deriving (Eq, Ord, Generic)

-- | Diatonic major intervals used to denote 'Chord' 'Addition's
data IntNat = I1  | I2  | I3  | I4 | I5 | I6 | I7 | I8 | I9 | I10 
            | I11 | I12 | I13 
  deriving (Eq, Enum, Ord, Bounded, Generic)     

type Interval = Note IntNat
  
-- | A musical note is a pitch (either absolute or relative) possibly modified
-- by an 'Accidental'
data Note a = Note Accidental a   deriving (Eq, Ord, Generic) 
  
-- | A musical 'Accidental'
data Accidental = Nat -- ^ natural
                | Sh  -- ^ sharp 
                | Fl  -- ^ flat
                | SS  -- ^ double sharp
                | FF  -- ^ double flat
  deriving (Eq, Ord, Generic)

-- | A 'Triad' comes in four flavours: major, minor, augmented, diminished, and 
-- sometimes a chord does not have a triad (e.g. suspended chords, etc.)
data Triad = MajTriad | MinTriad | AugTriad | DimTriad | NoTriad 
               deriving (Ord, Eq, Generic)
  
class (Generic a, Show a, Enum a, Bounded a) => Diatonic a

instance Diatonic DiatonicNatural
instance Diatonic DiatonicDegree
--------------------------------------------------------------------------------
-- Instances for the general music datatypes
--------------------------------------------------------------------------------   

instance Show Key where
  show (Key r m) = show r ++ show m
    
instance Show Mode where
  show MajMode = ""
  show MinMode = "m"  

-- In showing chords, we obey Harte et al.'s syntax as much as possible
instance Show ChordLabel where
  show NoChord    = "N"
  show UndefChord = "X"
  show (Chord r None []  b) = show r ++ ":1/" ++ show b
  show (Chord r sh   add b) = show r ++ ':' : show sh ++ showAdd add ++ showIv b
  -- show (Chord r None []  _loc _d) = show r ++ (if isRoot r then ":1" else "")
  -- show (Chord r None add _loc _d) = show r ++ ':' : showAdd add
  -- show (Chord r sh   add _loc _d) = show r ++ ':' : show sh ++ showAdd add
  -- show c = case chordRoot c of
     -- n@(Note Nat N) -> show n
     -- n@(Note Nat X) -> show n
     -- r                  -> show r ++ ':' : show (chordShorthand c) ++ ' ' : show ( toPitchClasses  c)   
    
showIv :: Interval -> String
showIv (Note Nat I1) = ""
showIv i             = '/' : show i 

    
showAdd :: [Addition] -> String
showAdd [] = ""
showAdd x  = '(' : intercalate "," (map show x) ++ ")"

instance Show Shorthand where
  show Maj      = "maj"
  show Min      = "min"
  show Dim      = "dim"
  show Aug      = "aug"
  show Maj7     = "maj7"
  show Min7     = "min7"
  show Sev      = "7"
  show Dim7     = "dim7"
  show HDim7    = "hdim7"
  show MinMaj7  = "minmaj7"
  show Aug7     = "aug7"
  show Maj6     = "maj6"
  show Min6     = "min6"
  show Maj9     = "maj9"
  show Min9     = "min9"
  show Min11    = "min11"
  show Min13    = "min13"
  show Maj13    = "maj13" 
  show Sus4     = "sus4"
  show Sus2     = "sus2"
  show SevSus4  = "7sus4"
  show Five     = "5" 
  show Nin      = "9" 
  show Eleven   = "11"
  show Thirteen = "13"
  show None     = ""
  
instance Show ClassType where
  show MajClass = ""
  show MinClass = "m"
  show DomClass = "7"
  show DimClass = "0"
  show NoClass  = "N"


instance Show (Note IntNat) where
  show (Note m i) = show m ++ show i

instance Show (Note DiatonicNatural) where
  show (Note m r) = show r ++ show m  
  
instance Show (Note DiatonicDegree) where
  show (Note m r) = show m ++ show r
  
instance Show IntNat where
  show a = show . ((!!) ([1..13]::[Integer])) 
                . fromJust $ elemIndex a [minBound..]
   
  
instance Show Accidental where 
  show Nat = ""
  show Sh  = "#"
  show Fl  = "b"
  show SS  = "##"
  show FF  = "bb"     

instance Show Addition where
  show (Add   n) = show n
  show (NoAdd n) = '*' : show n    

instance Show Triad where
  show MajTriad = "maj"
  show MinTriad = "min"
  show AugTriad = "aug"
  show DimTriad = "dim"
  show NoTriad  = "NoTriad"
  
--------------------------------------------------------------------------------
-- Tests     
--------------------------------------------------------------------------------

-- | Returns True if the 'ChordLabel' is not a chord, and False otherwise 
isNoneChord :: ChordLabel -> Bool
isNoneChord NoChord = True
isNoneChord _       = False

-- | Returns true if the 'Chord' 'Addition' represents an addition and not 
-- a degree that has to be removed (*).
isAddition :: Addition -> Bool
isAddition (Add   _) = True
isAddition (NoAdd _) = False

--------------------------------------------------------------------------------
-- Transformation and analysis of chords
--------------------------------------------------------------------------------

-- | Returns the 'ClassType' given a 'Chord'. This function uses 
-- 'analyseDegClassType' to analyse a chord and derive the 'ClassType'
toClassType :: Chord a -> ClassType
toClassType (Chord  _r  sh []   _b) = shToClassType sh -- no additions
-- combine the degrees and analyse them. N.B., also NoAdd degrees are resolved
toClassType c = undefined --analyseDegClassType . toIntValList $ c

-- | Analyses a degree list and returns 'MajTriad', 'MinTriad' or 'NoTriad' if
-- the degrees make a chord a major, minor, or no triad, respectively.
analyseDegClassType :: [Interval] -> ClassType
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
toTriad c = undefined -- analyseDegTriad . toIntValList $ c

-- | Analyses a degree list and returns 'MajTriad', 'MinTriad' or 'NoTriad' if
-- the degrees make a chord a major, minor, or no triad, respectively.
analyseDegTriad :: [Interval] -> Triad
analyseDegTriad degs =  
    case (analyseThird degs, analyseFifth degs) of
       (MajThird, PerfFifth) -> MajTriad
       (MajThird, AugFifth ) -> AugTriad
       (MajThird, DimFifth ) -> NoTriad
       (MinThird, PerfFifth) -> MinTriad
       (MinThird, AugFifth ) -> NoTriad
       (MinThird, DimFifth ) -> DimTriad
       (NoThird,  _        ) -> NoTriad
       (_      ,  NoFifth  ) -> NoTriad
      
-- analyses the third in a degree list
analyseThird :: [Interval] -> Third
analyseThird d 
  | (Note  Nat I3) `elem` d = MajThird
  | (Note  Fl  I3) `elem` d = MinThird
  | (Note  Fl  I4) `elem` d = MajThird
  | (Note  Sh  I2) `elem` d = MinThird
  | otherwise                    = NoThird
      
-- analyses the fifth in a degree list 
analyseFifth :: [Interval] -> Fifth
analyseFifth d  
  | (Note Nat  I5) `elem` d = PerfFifth
  | (Note Fl I5) `elem` d = DimFifth
  | (Note Sh I5) `elem` d = AugFifth
  | (Note Sh I4) `elem` d = DimFifth
  | (Note Fl I6) `elem` d = AugFifth
  | otherwise                    = NoFifth

-- analyses the fifth in a degree list 
analyseSevth :: [Interval] -> Sevth
analyseSevth d  
  | (Note Nat I7) `elem` d = MajSev
  | (Note Fl I7) `elem` d = MinSev
  | (Note FF I7) `elem` d = DimSev
  | (Note Nat I6) `elem` d = DimSev
  | (Note Sh I6) `elem` d = MinSev
  | (Note SS I6) `elem` d = MajSev
  | (Note Fl I8) `elem` d = MajSev
  | (Note FF I8) `elem` d = MinSev
  | otherwise                    = NoSev
 
 
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

-- | We hide the constructors, such that a PCSet can only be constructed with
-- 'toPitchClasses', this to overcome confusion between interval sets and
-- pitch class sets, which are both 'IntSet's
newtype PCSet = PCSet {pc :: IntSet} deriving (Show, Eq)

-- | Similar to 'toIntValList' but returns 'Int' pitch classes and includes the
-- 'Root' note of the the 'Chord'.
toPitchClasses :: ChordLabel -> PCSet
toPitchClasses c = PCSet . intSetToPC ivs . chordRoot $ c
  where ivs = toIntValList c `union` fromList [0, toIntervalClss (chordBass c)]

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
toIntValList :: Chord a -> IntSet
toIntValList (Chord  _r sh [] _b) = shToIntSet sh
toIntValList (Chord  _r sh a  _b) = shToIntSet sh `union` addToIntSet a
toIntValList _ = error ("HarmTrace.Base.MusicRep.toIntValList: cannot create" ++
                        "interval list for N or X")

addToIntSet :: [Addition] -> IntSet 
addToIntSet add = toSet adds S.\\ toSet remv

  where (adds, remv) = partition isAddition add

        toSet :: [Addition] -> IntSet
        toSet = fromList . map (toIntervalClss . getInt)
        
        getInt :: Addition -> Interval
        getInt (NoAdd i) = i
        getInt (Add   i) = i
        
        
addToIntValList :: [Addition] -> [Interval]  
addToIntValList add =  map toIntVal (adds \\ (toAdds remv))
  
  where (adds, remv) = partition isAddition add

        toAdds :: [Addition] -> [Addition]
        toAdds = map (\(NoAdd x) -> (Add x))
        
        toIntVal :: Addition -> Interval
        toIntVal (Add i) = i
        toIntVal _ = error "Cannot transform NoAdd to Interval" -- cannot happen
  
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
-- >>> intValToPitch (Note Sh G) (Note Fl I13)
-- >>> E
--  
-- >>> intValToPitch (Note Nat C) (Note Sh I11)
-- >>> F#
--
intValToPitch :: Root -> Interval -> Root
intValToPitch r = toRoot . intValToPitchClss r
 
-- | As 'intValToPitch', but returns the 'Int' pitch class. 
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

intSetToPC :: IntSet -> Root -> IntSet
intSetToPC is r = S.map (transp (toPitchClass r)) is

transp :: Int -> Int -> Int
transp t i = (i + t) `mod` 12

toChord :: IntSet -> Root -> Chord Root
toChord is r = Chord r None add (Note Nat I1)
  where add = map (Add . toInterval) . toAscList $ intSetToPC is r

toInterval :: Int -> Interval
toInterval i
  | 0 <= i && i <= 21 = intervals !! i
  | otherwise         = error ("HarmTrace.Base.MusicRep.toInterval " ++
                               "invalid pitch class: " ++ show i)

-- | The reverse of 'toPitchClass' returning the 'Note DiatonicNatural' given a 
-- Integer [0..11] semitone, where 0 represents C. When the integer is out 
-- of the range [0..11] an error is thrown.
toRoot :: Int -> Root
toRoot i 
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
--------------------------------------------------------------------------------
-- Binary instances
--------------------------------------------------------------------------------

instance Binary Key
instance Binary Mode 
instance Binary PieceLabel
instance Binary a => Binary (Chord a)
instance Binary ClassType
instance Binary Shorthand
instance Binary DiatonicDegree
instance Binary DiatonicNatural
instance Binary Addition
instance Binary IntNat
instance Binary a => Binary (Note a)
instance Binary Accidental
instance Binary Triad

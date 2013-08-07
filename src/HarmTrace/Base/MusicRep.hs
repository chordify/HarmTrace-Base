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
  -- ** Keys
  , Key (..)
  , Mode (..)
  -- ** Chords
  , Chord (..)
  -- , Class
  , Shorthand (..)
  , Addition (..)
  , Interval (..)
  , ChordLabel
  , ChordDegree
  , noneLabel
  , unknownLabel
  -- * Derived types for classification of chords
  , ClassType (..)
  , Triad (..)
  -- * Tests
  , isNone
  , isNoneChord
  , isUnknown
  , isRoot
  , isAddition
  -- * Transformation and analysis of chords
  , toClassType
  , toTriad
  , analyseDegTriad
  , toDegreeList
  , toMode
  , toMajMin
  , toMajMinChord
  , simplifyRoot
  -- * Scale degree transposition
  , toChordDegree
  , toScaleDegree
  , transposeSem
  , toSemitone
  , toIntervalClss
  , toRoot
  ) where
  
import Data.Maybe (fromJust)
import Data.List (elemIndex, intercalate, (\\), partition)
import Data.Binary (Binary)
import GHC.Generics (Generic)

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

-- rename to noLabel?
-- | No Chord label
noneLabel :: ChordLabel
noneLabel = (Chord (Note Nothing N) None [] 0 1)

-- | Unknown Chord label
unknownLabel :: ChordLabel
unknownLabel = (Chord (Note Nothing X) None [] 0 1)

-- | A chord based on relative 'ScaleDegree's
type ChordDegree  = Chord ScaleDegree

-- | The representation for a single chord 
data Chord a = Chord { chordRoot        :: a
                     , chordShorthand   :: Shorthand
                     , chordAdditions   :: [Addition]
                     -- | the index of the chord in the list of tokens
                     , getLoc           :: Int 
                     -- | the duration of the chord 
                     , duration         :: Int 
                     } deriving Generic

-- | We introduce four chord categories: major chords, minor chords, dominant
-- seventh chords, and diminshed seventh chords
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
                 -- In some cases there is no chord a certain position
                 -- This is especially important for the chroma processing
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
                     |  N -- ^ for no root
                     |  X -- ^ for representing unknown roots (used in MIREX)
  deriving (Show, Eq, Enum, Ord, Bounded, Generic)
  
-- | Intervals for additional chord notes    
data Addition = Add   (Note Interval)
              | NoAdd (Note Interval) deriving (Eq, Ord, Generic)

-- | Diatonic major intervals used to denote 'Chord' 'Addition's
data Interval = I1  | I2  | I3  | I4 | I5 | I6 | I7 | I8 | I9 | I10 
              | I11 | I12 | I13 
  deriving (Eq, Enum, Ord, Bounded, Generic)     

-- | A musical note is a pitch (either absolute or relative) possibly modified
-- by an 'Accidental'
data Note a = Note (Maybe Accidental) a   deriving (Eq, Ord, Generic) 
  
-- | A musical 'Accidental'
data Accidental = Sh -- ^ sharp 
                | Fl -- ^ flat
                | SS -- ^ double sharp
                | FF -- ^ double flat
  deriving (Eq, Ord, Generic)

-- | A 'Triad' comes in four flavours: major, minor, augmented, diminished, and 
-- sometimes a chord does not have a triad (e.g. suspended chords, etc.)
data Triad = MajTriad | MinTriad | AugTriad | DimTriad | NoTriad 
               deriving (Ord, Eq, Generic)
  
class (Show a, Enum a, Bounded a) => Diatonic a

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

-- TODO: it is probably best to derive eq for Chord and define other eq's 
-- for specific tasks if needed.  
instance Eq a => Eq (Chord a) where
  (Chord ra sha dega _loc _d) == (Chord rb shb degb _locb _db) 
     = ra == rb && sha == shb && dega == degb 

-- In showing chords, we obey Harte et al.'s syntax as much as possible
instance Show ChordLabel where
  show (Chord r None []  _loc _d) = show r ++ (if isRoot r then ":1" else "")
  show (Chord r None add _loc _d) = show r ++ ':' : showAdd add
  show (Chord r sh   add _loc _d) = show r ++ ':' : show sh ++ showAdd add
  
  
instance Show ChordDegree where
  show (Chord r None []  _loc _d) = show r ++ ":1"
  show (Chord r None add _loc _d) = show r ++ ':' : showAdd add
  show (Chord r sh   add _loc _d) = show r ++ ':' : show sh ++ showAdd add  
    
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


instance Show (Note Interval) where
  show (Note m i) = maybe "" show m ++ show i

instance Show (Note DiatonicNatural) where
  show (Note m r) = show r ++ maybe "" show m  
  
instance Show (Note DiatonicDegree) where
  show (Note m r) = show r ++ maybe "" show m  
  
instance Show Interval where
  show a = show . ((!!) ([1..13]::[Integer])) 
                . fromJust $ elemIndex a [minBound..]
   
  
instance Show Accidental where 
  show Sh = "#"
  show Fl = "b"
  show SS = "##"
  show FF = "bb"     

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
-- | Returns True if the 'Root' is not unknown or none
isRoot :: Root -> Bool
isRoot r | isNone r    = False
         | isUnknown r = False
         | otherwise   = True

-- | Returns True if the 'Root' is 'N', and False otherwise 
isNone :: Root -> Bool
isNone (Note _ N) = True
isNone  _         = False

-- | Returns True if the 'ChordLabel' is not a chord, and False otherwise 
isNoneChord :: ChordLabel -> Bool
isNoneChord = isNone . chordRoot

-- | Returns True if the 'Root' is unknown, and False otherwise 
isUnknown :: Root -> Bool
isUnknown (Note _ X) = True
isUnknown _          = False

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
toClassType (Chord  _r  sh []   _loc _d) = shToClassType sh -- no additions
-- combine the degrees and analyse them. N.B., also NoAdd degrees are resolved
toClassType c = analyseDegClassType . toDegreeList $ c

-- | Analyses a degree list and returns 'MajTriad', 'MinTriad' or 'NoTriad' if
-- the degrees make a chord a major, minor, or no triad, respectively.
analyseDegClassType :: [Addition] -> ClassType
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
-- >>> toTriad (Chord (Note Nothing C) Min [NoAdd (Note (Just Fl) I3),Add (Note Nothing I3)] 0 0)
-- maj 
--
-- >>> toTriad (Chord (Note Nothing C) HDim7 [Add (Note (Just Sh) I11)] 0 0)
-- dim
--
-- >>> toTriad (Chord (Note Nothing C) Min [NoAdd (Note (Just Fl) I3)] 0 0)
-- NoTriad
--
toTriad :: Chord a -> Triad
toTriad (Chord  _r  sh []   _loc _d) = shToTriad sh -- there are no additions
-- combine the degrees and analyse them. N.B., also NoAdd degrees are resolved
toTriad c = analyseDegTriad . toDegreeList $ c

-- | Analyses a degree list and returns 'MajTriad', 'MinTriad' or 'NoTriad' if
-- the degrees make a chord a major, minor, or no triad, respectivly.
analyseDegTriad :: [Addition] -> Triad
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
analyseThird :: [Addition] -> Third
analyseThird d 
  | (Add (Note  Nothing  I3)) `elem` d = MajThird
  | (Add (Note (Just Fl) I3)) `elem` d = MinThird
  | (Add (Note (Just Fl) I4)) `elem` d = MajThird
  | (Add (Note (Just Sh) I2)) `elem` d = MinThird
  | otherwise                          = NoThird
      
-- analyses the fifth in a degree list 
analyseFifth :: [Addition] -> Fifth
analyseFifth d  
  | (Add (Note  Nothing  I5)) `elem` d = PerfFifth
  | (Add (Note (Just Fl) I5)) `elem` d = DimFifth
  | (Add (Note (Just Sh) I5)) `elem` d = AugFifth
  | (Add (Note (Just Sh) I4)) `elem` d = DimFifth
  | (Add (Note (Just Fl) I6)) `elem` d = AugFifth
  | otherwise                          = NoFifth

-- analyses the fifth in a degree list 
analyseSevth :: [Addition] -> Sevth
analyseSevth d  
  | (Add (Note  Nothing  I7)) `elem` d = MajSev
  | (Add (Note (Just Fl) I7)) `elem` d = MinSev
  | (Add (Note (Just FF) I7)) `elem` d = DimSev
  | (Add (Note Nothing   I6)) `elem` d = DimSev
  | (Add (Note (Just Sh) I6)) `elem` d = MinSev
  | (Add (Note (Just SS) I6)) `elem` d = MajSev
  | (Add (Note (Just Fl) I8)) `elem` d = MajSev
  | (Add (Note (Just FF) I8)) `elem` d = MinSev
  | otherwise                          = NoSev
 
 
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

toPitchClasses :: Chord Root -> [Int]
toPitchClasses = undefined

-- | Transforms a Chord into a list of relative degrees (i.e. 'Addition's,
-- without the root note).
-- 
-- >>> toDegreeList (Chord (Note Nothing C) HDim7 [Add (Note (Just Sh) I11)] 0 0)
-- [3b,5b,7b,11#]
--
-- >>> toDegreeList (Chord (Note Nothing C) Min13 [NoAdd (Note Nothing I11)] 0 0)
-- [3b,5,7b,9,13]
--
-- >>> toDegreeList (parseData pChord "D:7(b9)")
-- [3,5,7b,9b]
--
toDegreeList :: Chord a -> [Addition]
toDegreeList (Chord  _r sh []  _loc _d) = map Add (shToDeg sh)
toDegreeList (Chord  _r sh deg _loc _d) = adds  \\ (toAdds remv) where

  (adds, remv) = partition isAddition ((map Add . shToDeg $ sh) ++ deg)

  toAdds :: [Addition] -> [Addition]
  toAdds = map (\(NoAdd x) -> (Add x))


  
-- | Expands a 'Shorthand' to its list of degrees
shToDeg :: Shorthand -> [Note Interval]     
shToDeg Maj     = [Note Nothing   I3, Note Nothing   I5]
shToDeg Min     = [Note (Just Fl) I3, Note Nothing   I5]
shToDeg Dim     = [Note (Just Fl) I3, Note (Just Fl) I5]
shToDeg Aug     = [Note Nothing   I3, Note (Just Sh) I5]
shToDeg Maj7    = shToDeg Maj     ++ [Note Nothing   I7]
shToDeg Min7    = shToDeg Min     ++ [Note (Just Fl) I7]
shToDeg Sev     = shToDeg Maj     ++ [Note (Just Fl) I7]
shToDeg Dim7    = shToDeg Dim     ++ [Note (Just FF) I7]
shToDeg HDim7   = shToDeg Dim     ++ [Note (Just Fl) I7]
shToDeg MinMaj7 = shToDeg Min     ++ [Note Nothing   I7]
shToDeg Aug7    = shToDeg Aug     ++ [Note (Just Fl) I7]
shToDeg Maj6    = shToDeg Maj     ++ [Note Nothing   I6]
shToDeg Min6    = shToDeg Min     ++ [Note (Just Fl) I6]
shToDeg Nin     = shToDeg Sev     ++ [Note Nothing   I9]
shToDeg Maj9    = shToDeg Maj7    ++ [Note Nothing   I9]
shToDeg Min9    = shToDeg Min7    ++ [Note Nothing   I9]
shToDeg Five    = [Note Nothing   I5]
shToDeg Sus2    = [Note Nothing   I2, Note Nothing   I5]
shToDeg Sus4    = [Note Nothing   I4, Note Nothing   I5]
shToDeg SevSus4 = shToDeg Sus4    ++ [Note (Just Fl) I7]
shToDeg None    = []
-- additional Billboard shorthands
shToDeg Min11    = shToDeg Min9   ++ [Note Nothing   I11]
shToDeg Eleven   = shToDeg Nin    ++ [Note Nothing   I11]
shToDeg Min13    = shToDeg Min11  ++ [Note Nothing   I13]
shToDeg Maj13    = shToDeg Maj9   ++ [Note Nothing   I13]
shToDeg Thirteen = shToDeg Eleven ++ [Note Nothing   I13]
     
      
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
toMajMinChord c = c {chordShorthand = majMinSh}
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
toChordDegree k (Chord r sh degs loc d) = 
                 Chord (toScaleDegree k r) sh degs loc d    
    
-- | Transformes a absolute 'Root' 'Note' into a relative 'ScaleDegree', given
-- a 'Key'.
toScaleDegree :: Key -> Root -> ScaleDegree
toScaleDegree _ n@(Note _ N) = 
  error ("HarmTrace.Base.MusicRep.toScaleDegree: cannot transpose " ++ show n)
toScaleDegree (Key kr _) cr  = -- Note Nothing I
  scaleDegrees!!(((toSemitone cr) - (toSemitone kr)) `mod` 12)

-- | Simplify note roots to a single enharmonic representation.
-- For instance, D♭ becomes C♯, E♯ becomes F, and G𝄫 becomes F.
simplifyRoot :: Root -> Root
-- Simplify double sharps
simplifyRoot (Note (Just SS) x) | x == E    = Note (Just Sh) F
                                | x == B    = Note (Just Sh) C
                                | otherwise = Note Nothing   (succ x)
-- Simplify double flats
simplifyRoot (Note (Just FF) x) | x == F    = Note (Just Fl) E
                                | x == C    = Note (Just Fl) B
                                | otherwise = Note Nothing   (pred x)
-- Simplify sharps
simplifyRoot (Note (Just Sh) D) = Note (Just Fl) E
simplifyRoot (Note (Just Sh) E) = Note Nothing   F
simplifyRoot (Note (Just Sh) G) = Note (Just Fl) A
simplifyRoot (Note (Just Sh) A) = Note (Just Fl) B
simplifyRoot (Note (Just Sh) B) = Note Nothing   C
-- Simplify flats
simplifyRoot (Note (Just Fl) C) = Note Nothing   B
simplifyRoot (Note (Just Fl) D) = Note (Just Sh) C
simplifyRoot (Note (Just Fl) F) = Note Nothing   E
simplifyRoot (Note (Just Fl) G) = Note (Just Sh) F
-- Everything else must be simple already
simplifyRoot x                  = x
  
-- | Transposes a scale degree with @sem@ semitones up
transposeSem :: ScaleDegree -> Int -> ScaleDegree
transposeSem deg sem = scaleDegrees!!((sem + (toSemitone deg)) `mod` 12) where

-- TODO : should be renamed to 'toPitchClass'
-- | Returns the semitone value [0 .. 11] of a 'ScaleDegree' where
-- 0 = C, e.g. F# = 6. For the constructors 'N' and 'X' an error is thrown.
toSemitone :: (Diatonic a) => Note a -> Int
toSemitone (Note m p) 
  | ix <= 6   = noNegatives (([0,2,4,5,7,9,11] !! ix) + modToSemi m) `mod` 12
  | otherwise = error ("HarmTrace.Base.MusicRep.toSemitone: no semitone for "
                        ++ show p ++ maybe "" show m )
      where ix = fromEnum p
            noNegatives s | s < 0     = 12 + s
                          | otherwise = s

-- | Similar to 'toPitchClss', this function calculates an enharmonic 
-- interval class for each 'Note Interval' in the range of [0 .. 23]
-- ( == ['Note Nothing I1' .. 'Note (Just SS) I13']
toIntervalClss :: Note Interval -> Int
toIntervalClss n@(Note m i) =
  --         1 2 3 4 5 6 7  8  9  10 11 12 13
  let ic = ([0,2,4,5,7,9,11,12,14,16,17,19,21] !! (fromEnum i)) + modToSemi m 
  in  if ic >= 0 then ic
                 else error ("HarmTrace.Base.MusicRep.toIntervalClss: no "
                          ++ "interval class for " ++ show n)
                          
-- | The reverse of 'toSemitone' returning the 'Note DiatonicNatural' given a 
-- Integer [0..11] semitone, where 0 represents C. When the integer is out 
-- of the range [0..11] an error is thrown.
toRoot :: Int -> Root
toRoot i 
  | 0 <= i && i <= 11 = roots !! i
  | otherwise         = error ("HarmTrace.Base.MusicRep.toRoot " ++
                               "invalid semitone: " ++ show i)
    
-- | Transforms type-level Accidentals to semitones (Int values)
modToSemi :: Maybe Accidental -> Int
modToSemi  Nothing  =  0
modToSemi (Just Sh) =  1
modToSemi (Just Fl) = -1
modToSemi (Just SS) =  2
modToSemi (Just FF) = -2

-- | A list of 12 'ScaleDegree's, ignoring pitch spelling.
scaleDegrees ::[ ScaleDegree ]  
scaleDegrees = [ Note  Nothing   I
               , Note  (Just Sh) I
               , Note  Nothing   II
               , Note  (Just Fl) III
               , Note  Nothing   III
               , Note  Nothing   IV
               , Note  (Just Sh) IV
               , Note  Nothing   V
               , Note  (Just Fl) VI
               , Note  Nothing   VI
               , Note  (Just Fl) VII
               , Note  Nothing   VII
               ]
               

-- | A list of 12 'Note DiatonicNatural's, ignoring pitch spelling.
roots :: [ Root ]  
roots =  [ Note Nothing   C
         , Note (Just Sh) C
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
instance Binary Interval
instance Binary a => Binary (Note a)
instance Binary Accidental
instance Binary Triad

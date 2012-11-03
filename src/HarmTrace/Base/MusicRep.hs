{-# OPTIONS_GHC -Wall           #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.MusicRep
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
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
  , isUnknownChord
  , isAddition
  -- * Transformation and analysis of chords
  , toClassType
  , toTriad
  , analyseDegTriad
  -- , analyseThird
  -- , analyseFifth
  -- , analyseSevth
  , toDegreeList
  , toMode
  , toMajMin
  , toMajMinChord
  -- * Scale degree transposition
  , toChordDegree
  , toScaleDegree
  , transposeSem
  , toSemitone
  -- , modeToSemi
  -- * Miscellaneous
  , scaleDegrees
  ) where
  
import Data.Maybe
import Data.List (elemIndex, intercalate, (\\), partition)

--------------------------------------------------------------------------------
-- Representing musical information at the value level
--------------------------------------------------------------------------------


-- | A musical key consising of a 'Root' and 'Mode'
data Key  = Key { keyRoot :: Root, keyMode :: Mode } deriving (Eq, Ord)

-- | The 'Mode' of a key, which can be major or minor
data Mode = MajMode | MinMode deriving (Eq, Ord)

-- | A container type combinint a key and a list of 'ChordLabel's
data PieceLabel = PieceLabel Key [ChordLabel]

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
                     }

-- | We introduce four chord categories: major chords, minor chords, dominant
-- seventh chords, and diminshed seventh chords
data ClassType = MajClass | MinClass | DomClass | DimClass | NoClass
  deriving (Eq, Enum, Ord, Bounded)

-- Following Harte et al., we define a number of chord 'Shorthand's
data Shorthand = -- | Triadic chords
                 Maj | Min | Dim | Aug
                 -- | Seventh chords
               | Maj7 | Min7 | Sev | Dim7 | HDim7 | MinMaj7
                 -- | Sixth chords
               | Maj6 | Min6
                 -- | Extended chords
               | Nin | Maj9 | Min9
                 -- | Suspended chords
               | Sus4 | Sus2
                 -- | Power chords
               | Five
                 -- In some cases there is no chord a certain position
                 -- This is especially important for the chroma processing
               | None
                 -- Additional shorthands in billboard collection
               | Eleven | Thirteen | Min11 | Maj13 | Min13
               
  deriving (Show, Eq, Ord, Enum, Bounded) 


-- | Key relative scale degrees to abstract from the absolute Root notes
type ScaleDegree = Note DiatonicDegree

-- | All Diatonic scale degrees 
data DiatonicDegree = I | II | III | IV | V | VI | VII 
                    | Imp -- ^ for unrepresentable scale degrees
  deriving (Show, Eq, Enum, Ord, Bounded)

-- | Representing absolute 'Root' notes  
type Root = Note DiatonicNatural

-- | The seven diatonic naturals
data DiatonicNatural =  C | D | E | F | G | A | B 
                     |  N -- ^ for no root
                     |  X -- ^ for representing unknown roots (used in MIREX)
  deriving (Show, Eq, Enum, Ord, Bounded)
  
-- | Intervals for additonal chord notes    
data Addition = Add   (Note Interval)
              | NoAdd (Note Interval) deriving (Eq, Ord)

-- | Diatonic major intervals used to denote 'Chord' 'Addition's
data Interval = I1  | I2  | I3  | I4 | I5 | I6 | I7 | I8 | I9 | I10 
              | I11 | I12 | I13 
  deriving (Eq, Enum, Ord, Bounded)     

-- | A musical note is a pitch (either absolute or relative) posibly modified
-- by an 'Accidental'
data Note a = Note (Maybe Accidental) a   deriving (Eq, Ord) 
  
-- | A musical 'Accidental'
data Accidental = Sh -- ^ sharp 
                | Fl -- ^ flat
                | SS -- ^ double sharp
                | FF -- ^ double flat
  deriving (Eq, Ord)

-- | A 'Triad' comes in four flavours: major, minor, augmented, dimished, and 
-- sometimes a chord does not have a triad (e.g. suspended chords, etc.)
data Triad = MajTriad | MinTriad | AugTriad | DimTriad | NoTriad 
               deriving (Ord, Eq)
  
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
  
instance (Show a) => Show (Chord a) where
  show (Chord r sh deg _loc _d) =  show r ++ ':' : show sh 
                            ++ (if not (null deg) then showAdds deg else "")
                            -- ++ '_' : show loc ++ ':' : show d

showAdds :: Show a => [a] -> String                                
showAdds x = '(' : intercalate "," (map show x) ++ ")"
                            
instance Show ClassType where
  show MajClass = ""
  show MinClass = "m"
  show DomClass = "7"
  show DimClass = "0"
  show NoClass  = "N"

instance (Show a) => Show (Note a) where
  show (Note m interval) = show interval ++ maybe "" show m

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

-- | Returns True if the 'Root' is 'N', and False otherwise 
isNone :: Root -> Bool
isNone (Note _ N) = True
isNone  _         = False

-- | Returns True if the 'ChordLabel' is not a chord, and False otherwise 
isNoneChord :: ChordLabel -> Bool
isNoneChord (Chord (Note _ N) _ _ _ _) = True
isNoneChord _                          = False

-- | Returns True if the 'Root' is unknown, and False otherwise 
isUnknown :: Root -> Bool
isUnknown (Note _ X) = True
isUnknown _          = False

-- | Returns True if the 'ChordLabel' is not a unknown, and False otherwise 
isUnknownChord :: ChordLabel -> Bool
isUnknownChord (Chord (Note _ X) _ _ _ _) = True
isUnknownChord (Chord (Note _ N) _ _ _ _) = False -- known to be NoneChord
isUnknownChord (Chord _ None _ _ _)       = True
isUnknownChord _                          = False

-- | Returns true if the 'Chord' 'Addition' represents an addition and not 
-- a degree that has to be removed (*).
isAddition :: Addition -> Bool
isAddition (Add   _) = True
isAddition (NoAdd _) = False

--------------------------------------------------------------------------------
-- Transformation and analysis of chords
--------------------------------------------------------------------------------
toClassType :: Chord a -> ClassType
toClassType (Chord  _r  sh []   _loc _d) = shToClassType sh -- no additions
-- combine the degrees and analyse them. N.B., also NoAdd degrees are resolved
toClassType c = analyseDegClassType . toDegreeList $ c

-- | Analyses a degree list and returns 'MajTriad', 'MinTriad' or 'NoTriad' if
-- the degrees make a chord a major, minor, or no triad, respectivly.
analyseDegClassType :: [Addition] -> ClassType
analyseDegClassType degs = 
    case (analyseThird degs, analyseFifth degs, analyseSevth degs) of
       -- Triads
       (_       , _        , MinSev) -> DomClass
       (_       , AugFifth , _     ) -> DomClass
       (MajThird, DimFifth , _     ) -> DomClass
       (MajThird, _        , _     ) -> MajClass
       (MinThird, PerfFifth, _     ) -> MinClass
       (MinThird, DimFifth , DimSev) -> DimClass
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
shToClassType Maj6    = MajClass 
shToClassType Min6    = MinClass
shToClassType Nin     = DomClass
shToClassType Maj9    = MajClass
shToClassType Min9    = MinClass
shToClassType Five    = NoClass
shToClassType Sus2    = NoClass
shToClassType Sus4    = NoClass
shToClassType None    = NoClass
-- additional Billboard shorthands
shToClassType Min11    = MinClass
shToClassType Eleven   = DomClass
shToClassType Min13    = MinClass
shToClassType Maj13    = MajClass
shToClassType Thirteen = DomClass

{-
-- | /Depricated/ Categorises a 'Shorthand' into a 'ClassType'.
toClassType' :: Shorthand -> ClassType
toClassType' sh -- TODO: reconsider these categories...
  | sh `elem` [Maj,Maj7,Maj6,Maj9,MinMaj7,Five,Sus4,Sus2] = MajClass
  | sh `elem` [Min,Min7,Min6,Min9,HDim7] = MinClass
  | sh `elem` [Sev,Nin,Aug] = DomClass
  | sh `elem` [Dim,Dim7] = DimClass 
  | otherwise = error 
      ("HarmTrace.Base.MusicRep.toClassType: unknown shorthand: " ++ show sh)
-}
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
  | (Add (Note (Just Fl) I3)) `elem` d = MinThird
  | (Add (Note  Nothing  I3)) `elem` d = MajThird
  | otherwise                          = NoThird
      
-- analyses the fifth in a degree list 
analyseFifth :: [Addition] -> Fifth
analyseFifth d  
  | (Add (Note (Just Fl) I5)) `elem` d = DimFifth
  | (Add (Note (Just Sh) I5)) `elem` d = AugFifth
  | (Add (Note  Nothing  I5)) `elem` d = PerfFifth
  | otherwise                          = NoFifth

-- analyses the fifth in a degree list 
analyseSevth :: [Addition] -> Sevth
analyseSevth d  
  | (Add (Note (Just FF) I7)) `elem` d = DimSev
  | (Add (Note (Just Fl) I7)) `elem` d = MinSev
  | (Add (Note  Nothing  I7)) `elem` d = MajSev
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
shToTriad Dim7    = MinTriad
shToTriad HDim7   = MinTriad
shToTriad MinMaj7 = MinTriad
shToTriad Maj6    = MajTriad 
shToTriad Min6    = MinTriad
shToTriad Nin     = MajTriad
shToTriad Maj9    = MajTriad
shToTriad Min9    = MinTriad
shToTriad Five    = NoTriad
shToTriad Sus2    = NoTriad
shToTriad Sus4    = NoTriad
shToTriad None    = NoTriad
-- additional Billboard shorthands
shToTriad Min11    = MinTriad
shToTriad Eleven   = MajTriad
shToTriad Min13    = MinTriad
shToTriad Maj13    = MajTriad
shToTriad Thirteen = MajTriad

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
shToDeg Maj6    = shToDeg Maj     ++ [Note Nothing   I6]
shToDeg Min6    = shToDeg Min     ++ [Note (Just Fl) I6]
shToDeg Nin     = shToDeg Sev     ++ [Note Nothing   I9]
shToDeg Maj9    = shToDeg Maj7    ++ [Note Nothing   I9]
shToDeg Min9    = shToDeg Min7    ++ [Note Nothing   I9]
shToDeg Five    = [Note Nothing   I5]
shToDeg Sus2    = [Note Nothing   I2, Note Nothing   I5]
shToDeg Sus4    = [Note Nothing   I4, Note Nothing   I5]
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
toMajMinChord :: Show a => Chord a -> Chord a
toMajMinChord c = c {chordShorthand = majMinSh}
  where majMinSh = case toMajMin (toTriad c) of
                     MajClass -> Maj
                     MinClass -> Min
                     NoClass  -> Min -- this is odd, but this is how it was
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

-- | Transposes a scale degree with @sem@ semitones up
transposeSem :: ScaleDegree -> Int -> ScaleDegree
transposeSem deg sem = scaleDegrees!!((sem + (toSemitone deg)) `mod` 12) where

-- | Returns the semitone value [0 .. 11] of a 'ScaleDegree', e.g. F# = 6  
toSemitone :: (Show a, Enum a) => Note a -> Int
toSemitone (Note m p)
  | ix > 6    = error ("HarmTrace.Base.MusicRep.toSemitone: no semitone for "
                        ++ show (Note m p))
  | otherwise = ([0,2,4,5,7,9,11] !! ix) + modToSemi m where
    ix = fromEnum p

-- | Transforms type-level Accidentals to semitones (Int values)
modToSemi :: Maybe Accidental -> Int
modToSemi  Nothing  =  0
modToSemi (Just Sh) =  1
modToSemi (Just Fl) = -1
modToSemi (Just SS) =  2
modToSemi (Just FF) = -2

-- | A list of 12 'ScaleDegree's, ignoring pitch spelling.
scaleDegrees ::[ScaleDegree]  
scaleDegrees = [ Note  Nothing   I
               , Note  (Just Fl) II
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
{-# OPTIONS_GHC -Wall             #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.MusicRep
-- Copyright   :  (c) 2012--2016, Chordify BV
-- License     :  LGPL-3
--
-- Maintainer  :  haskelldevelopers@chordify.net
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

module HarmTrace.Base.Chord.Datatypes (
  -- * Representing musical chords and keys
    Note (..)
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
  , chordRoot
  , chordShorthand
  , chordAdditions
  , chordBass
  , Shorthand (..)
  , Addition (..)
  , IntNat (..)
  , Interval
  , ChordLabel
  , ChordDegree

  -- ** Derived types for classification of chords
  , ClassType (..)
  , Triad (..)
  -- * Tests & Utilities
  , shortChord
  , discardBass
  , addition
  , insertAdd
  , isNoneChord
  , isAddition
  , catchNoChord
  , toHarte
  ) where

import Data.Maybe                 ( fromJust )
import Data.List                  ( elemIndex, intercalate, insert, delete )
import Data.Binary                ( Binary )
import GHC.Generics               ( Generic )
import Control.DeepSeq            ( NFData )

--------------------------------------------------------------------------------
-- Representing musical information at the value level
--------------------------------------------------------------------------------


-- | A musical key consising of a 'Root' and 'Mode'
data Key  = Key { keyRoot :: Root, keyMode :: Mode } deriving (Eq, Ord, Generic)

-- | The 'Mode' of a key, which can be major or minor
data Mode = MajMode | MinMode deriving (Eq, Ord, Generic)

-- | A chord based on absolute 'Root' notes
type ChordLabel   = Chord Root

-- | A chord based on relative 'ScaleDegree's
type ChordDegree  = Chord ScaleDegree

-- | The representation for a single chord consisting of a root, a 'Shorthand'
-- representing the interval structure of the chord, a list of 'Additions',
-- for representing other (additional) structure, and the base 'Inversion'
data Chord a = Chord a  Shorthand [Addition] Interval  -- ^ a regular chord
             | NoChord           -- ^ No sounding chord (silence, noise, etc.)
             | UndefChord        -- ^ An undefined chord
                deriving (Eq, Ord, Generic, Functor)

-- | Returns the root of a 'Chord', and throws an error in case of a 'NoChord'
-- or an 'UndefChord'.
chordRoot :: Show a => Chord a -> a
chordRoot = catchNoChord "Chord.Datatypes.chordRoot" (\(Chord r _ _ _) -> r)

-- | Returns the 'Shorthand' of a 'Chord', and throws an error in case of
-- a 'NoChord' or an 'UndefChord'.
chordShorthand :: Show a => Chord a -> Shorthand
chordShorthand = catchNoChord "Chord.Datatypes.chordShorthand" (\(Chord _ s _ _) ->s)

-- | Returns the list of 'Additions' of a 'Chord', and throws an error in case
-- of a 'NoChord' or an 'UndefChord'.
chordAdditions :: Show a => Chord a -> [Addition]
chordAdditions = catchNoChord "Chord.Datatypes.chordAdditions" (\(Chord _ _ a _) ->a)

-- | Returns the bass 'Interval' of a 'Chord', and throws an error in case of
-- a 'NoChord' or an 'UndefChord'.
chordBass :: Show a => Chord a -> Interval
chordBass = catchNoChord "Chord.Datatypes.chordBass" (\(Chord _ _ _ b) -> b)

-- | Updates the root field of a 'Chord'
-- updateRoot :: Chord a -> a -> Chord a
-- updateRoot (Chord r sh a b) r' = Chord r' sh a b

-- | We introduce four chord categories: major chords, minor chords, dominant
-- seventh chords, and diminished seventh chords
data ClassType = MajClass | MinClass | DomClass | DimClass | NoClass
  deriving (Eq, Enum, Ord, Bounded, Generic)

-- | Following Harte et al., we define a number of chord 'Shorthand's. We 
-- support a few extra shorthand, but the show intance of 'Shorthand' will only
-- output the 'Shorthand's that are in the official specification
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
                 -- | Only a root note
               | None
                 -- | Additional shorthands in billboard collection
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
  deriving (Show, Eq, Enum, Ord, Bounded, Generic)

-- | Intervals for additional chord notes
data Addition = Add   Interval
              | NoAdd Interval deriving (Eq, Ord, Generic)

-- | Diatonic major intervals used to denote 'Chord' 'Addition's and bass
-- 'Interval's
data IntNat = I1  | I2  | I3  | I4 | I5 | I6 | I7 | I8 | I9 | I10
            | I11 | I12 | I13
  deriving (Eq, Enum, Ord, Bounded, Generic)

-- \ Represents a musical interval
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

--------------------------------------------------------------------------------
-- Instances for the general music datatypes
--------------------------------------------------------------------------------

instance Read DiatonicNatural where
  readsPrec _ ('A':xs) = [(A, xs)]
  readsPrec _ ('B':xs) = [(B, xs)]
  readsPrec _ ('C':xs) = [(C, xs)]
  readsPrec _ ('D':xs) = [(D, xs)]
  readsPrec _ ('E':xs) = [(E, xs)]
  readsPrec _ ('F':xs) = [(F, xs)]
  readsPrec _ ('G':xs) = [(G, xs)]
  readsPrec _ _        = []

instance Show Key where
  show (Key r m) = show r ++ show m

instance Read Key where
  readsPrec i xs =
    [ (Key r m, zs)
    | (r, ys) <- readsPrec i xs
    , (m, zs) <- readsPrec i ys
    ]

instance Show Mode where
  show MajMode = ""
  show MinMode = "m"

instance Read Mode where
  readsPrec _ ('m':xs) = [(MinMode, xs)]
  readsPrec _      xs  = [(MajMode, xs)]

-- In showing chords, we obey Harte et al.'s syntax as much as possible
instance Show ChordLabel where
  show NoChord    = "N"
  show UndefChord = "X"
  show (Chord r None []  b) = show r ++ ":1" ++ showIv b
  show c = let (Chord r sh add b) = toHarte c
           in show r ++ ':' : show sh ++ showAdd add ++ showIv b

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
  show Maj6     = "maj6"
  show Min6     = "min6"
  show Nin      = "9"
  show Maj9     = "maj9"
  show Min9     = "min9"
  show Sus4     = "sus4"
  
  -- not part of the official Harte specification  
  show Aug7     = "aug7"
  show Min11    = "min11"
  show Min13    = "min13"
  show Maj13    = "maj13"
  show Sus2     = "sus2"
  show SevSus4  = "7sus4"
  show Five     = "5"
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

instance Read (Note DiatonicNatural) where
  readsPrec i xs =
    [ (Note m r, zs)
    | (r, ys) <- readsPrec i xs
    , (m, zs) <- readsPrec i ys
    ]

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

instance Read Accidental where
  readsPrec _ ('#':'#':xs) = [(SS, xs)]
  readsPrec _ (    '#':xs) = [(Sh, xs)]
  readsPrec _ ('b':'b':xs) = [(FF, xs)]
  readsPrec _ (    'b':xs) = [(Fl, xs)]
  readsPrec _          xs  = [(Nat, xs)]

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
-- Utilities
--------------------------------------------------------------------------------

-- | A Constructor for a simple chord based on a 'Root' and 'Shorthand' only
shortChord :: Root -> Shorthand -> ChordLabel
shortChord r sh = Chord r sh [] (Note Nat I1)

-- | Returns True if the 'ChordLabel' is not a chord, and False otherwise
isNoneChord :: ChordLabel -> Bool
isNoneChord NoChord = True
isNoneChord _       = False

-- | Returns true if the 'Chord' 'Addition' represents an addition and not
-- a degree that has to be removed (*).
isAddition :: Addition -> Bool
isAddition (Add   _) = True
isAddition (NoAdd _) = False

-- | Adds an 'Addition' to a 'Chord'
addition :: Chord a -> Addition -> Chord a
addition NoChord            _ = NoChord
addition UndefChord         _ = UndefChord
addition (Chord r sh ads b) a = Chord r sh (insertAdd ads a) b 

-- | Applies an 'Addition' to a list of 'Addition's
insertAdd :: [Addition] -> Addition -> [Addition]
insertAdd l (Add   a) = insert (Add a) l
insertAdd l (NoAdd r) = delete (Add r) l


-- | Discards a base note by replacing the bass 'Interval' by a
-- 'Note' 'Nat' 'I1'
discardBass :: Chord a -> Chord a
discardBass NoChord           = NoChord
discardBass UndefChord        = UndefChord
discardBass (Chord r sh a _b) = Chord r sh a (Note Nat I1)

-- | Checks if the 'ChordLabel' is a 'NoChord' or 'UndefChord' and throws
-- an error using the first argument as an function identifier for debugging.
-- In case of a 'ChordLabel' the second argument is applied to the third
-- argument.
catchNoChord :: Show a => String -> (Chord a -> b) ->  Chord a -> b
catchNoChord s f c = case c of
       NoChord    -> error ("HarmTrace.Base."++s++" applied to a NoChord")
       UndefChord -> error ("HarmTrace.Base."++s++" applied to a UndefChord")
       _          -> f c

-- | HarmTrace-Base accepts a larger vacabulary of 'Shorthand's then in specified
-- in the original Harte specification. This function ensures that a chord is
-- compatible with Harte syntax.
toHarte :: Chord a -> Chord a 
toHarte NoChord          = NoChord
toHarte UndefChord       = UndefChord
toHarte (Chord r sh a b) = let (sh', x) = toHarteSh sh
                               a'       = foldl (insertAdd) a x
                            -- in show r ++ ':' : show sh' ++ showAdd add' ++ showIv b
                           in Chord r sh' a' b

toHarteSh :: Shorthand -> (Shorthand, [Addition])
toHarteSh c = case c of
  Aug7     -> (Aug,  [Add (Note Fl  I7 )])
  Min11    -> (Min9, [Add (Note Nat I11)])
  Min13    -> (Min9, [Add (Note Nat I11), Add (Note Nat I13)])
  Maj13    -> (Maj9, [Add (Note Nat I11), Add (Note Nat I13)])
  Sus2     -> (None, [Add (Note Nat I2 ), Add (Note Nat I5)])
  SevSus4  -> (Sus4, [Add (Note Fl I7)])
  Five     -> (None, [Add (Note Nat I5)])
  Eleven   -> (Nin,  [Add (Note Nat I11)])
  Thirteen -> (Nin,  [Add (Note Nat I11),Add (Note Nat I13)])
  sh       -> (sh, [])     
  
  
  
--------------------------------------------------------------------------------
-- Binary instances
--------------------------------------------------------------------------------

instance Binary Key
instance Binary Mode
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

--------------------------------------------------------------------------------
-- NFData
--------------------------------------------------------------------------------

instance NFData Key
instance NFData Mode
instance NFData a => NFData (Chord a)
instance NFData ClassType
instance NFData Shorthand
instance NFData DiatonicDegree
instance NFData DiatonicNatural
instance NFData Addition
instance NFData IntNat
instance NFData a => NFData (Note a)
instance NFData Accidental
instance NFData Triad

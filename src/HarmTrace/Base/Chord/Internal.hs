{-# OPTIONS_GHC -Wall             #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Chord.Internal
--- Copyright   :  (c) 2012--2016, Chordify BV
-- License     :  LGPL-3
--
-- Maintainer  :  haskelldevelopers@chordify.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Some common function that are not exported
--------------------------------------------------------------------------------

module HarmTrace.Base.Chord.Internal (
    modToInt
  , scaleDegrees
  , roots
  , intervals
  ) where

import HarmTrace.Base.Chord.Datatypes

-- | Transforms type-level Accidentals to semitones (Int values)
modToInt :: Accidental -> Int
modToInt Nat =  0
modToInt Sh  =  1
modToInt Fl  = -1
modToInt SS  =  2
modToInt FF  = -2

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
roots ::       [ Root ]
roots =        [ Note Nat C
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


intervals ::   [ Interval ]
intervals =    [ Note Nat I1  --  0: Prime
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

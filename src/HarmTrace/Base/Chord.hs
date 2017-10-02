{-# OPTIONS_GHC -Wall             #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Chord
-- Copyright   :  (c) 2012--2016, Chordify BV
-- License     :  LGPL-3
--
-- Maintainer  :  haskelldevelopers@chordify.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: An interface to all the sub modules of HarmTrace.Base.Chord
--------------------------------------------------------------------------------

module HarmTrace.Base.Chord (
    module HarmTrace.Base.Chord.Datatypes
  , module HarmTrace.Base.Chord.Analysis
  , module HarmTrace.Base.Chord.PitchClass
  , module HarmTrace.Base.Chord.Intervals
  -- * Alternative Chord Printing
  , showChordWithNoteInversion
  ) where

import HarmTrace.Base.Chord.Datatypes
import HarmTrace.Base.Chord.Analysis
import HarmTrace.Base.Chord.PitchClass
import HarmTrace.Base.Chord.Intervals

import Data.List                       ( intercalate )


--------------------------------------------------------------------------------
-- Alternative printing
--------------------------------------------------------------------------------

-- | The Show instance obides the Harte syntax, but it can be more convenient
-- to show a chord with the inversion printed as a 'Note' instead of an 
-- 'Interval'
showChordWithNoteInversion :: ChordLabel -> String
showChordWithNoteInversion c = 
  let showIv :: Root -> Interval -> String
      showIv _ (Note Nat I1) = ""
      showIv r i             = '/' : show (intervalToPitch r i)
      
      showAdd :: [Addition] -> String
      showAdd [] = ""
      showAdd x  = '(' : intercalate "," (map show x) ++ ")"
  in case c of 
       NoChord    -> "N"
       UndefChord -> "X"
       (Chord r None []  b) -> show r ++ ":1" ++ showIv r b
       (Chord r sh   add b) -> show r ++ ':' : show sh ++ showAdd add ++ showIv r b 

{-# OPTIONS_GHC -Wall             #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Chord
-- Copyright   :  (c) 2013--2014 W. Bas de Haas and Jose Pedro Magalhaes,
--                Multiphonyx Holding BV
-- License     :  LGPL-3
--
-- Maintainer  :  bas@chordify.net, dreixel@chordify.net 
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
  ) where

import HarmTrace.Base.Chord.Datatypes
import HarmTrace.Base.Chord.Analysis
import HarmTrace.Base.Chord.PitchClass
import HarmTrace.Base.Chord.Intervals

{-# OPTIONS_GHC -Wall             #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveGeneric        #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Chord.PitchClass
-- Copyright   :  (c) 2013 W. Bas de Haas and Jose Pedro Magalhaes
-- License     :  LGPL-3
--
-- Maintainer  :  bas@chordify.net, dreixel@chordify.net 
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: 
--------------------------------------------------------------------------------
module HarmTrace.Base.Chord.PitchClass (
    PCSet  -- ^ Pitch Class Set
  , pc     -- ^ Unwraps a 'PCSet'
    -- * Pitch classes
  , toPitchClass
  , toPitchClasses
  , intValToPitchClss
  , intSetToPC
  , pcToRoot
  , EnHarEq (..)
  , Diatonic
  ) where

import HarmTrace.Base.Chord.Datatypes
import HarmTrace.Base.Chord.Intervals
import HarmTrace.Base.Chord.Internal

import Data.Binary                ( Binary )
import Data.IntSet                ( fromList, union )
import qualified Data.IntSet as S ( map )
import GHC.Generics               ( Generic )

-- | We hide the constructors, such that a PCSet can only be constructed with
-- 'toPitchClasses', this to overcome confusion between interval sets and
-- pitch class sets, which are both 'IntSet's
newtype PCSet = PCSet {pc :: IntSet} deriving (Show, Eq, Generic)

instance Binary PCSet

-- | Returns the semitone value [0 .. 11] of a 'ScaleDegree' where
-- 0 = C, e.g. F# = 6. For the constructors 'N' and 'X' an error is thrown.
toPitchClass :: (Diatonic a) => Note a -> Int
toPitchClass (Note m p) 
  | ix <= 6   = noNegatives (([0,2,4,5,7,9,11] !! ix) + modToInt m) `mod` 12
  | otherwise = error ("HarmTrace.Base.MusicRep.toPitchClass: no semitone for "
                        ++ show p ++ show m )
      where ix = fromEnum p
            noNegatives s | s < 0     = 12 + s
                          | otherwise = s
                          
-- | Similar to 'toIntValList' but returns 'Int' pitch classes and includes the
-- 'Root' note of the the 'Chord'.
toPitchClasses :: ChordLabel -> PCSet
toPitchClasses c = intSetToPC ivs . chordRoot $ c
  where ivs = toIntSet c `union` fromList [0, toIntervalClss (chordBass c)]

  
intSetToPC :: IntSet -> Root -> PCSet
intSetToPC is r = PCSet . S.map (transp (toPitchClass r)) $ is where

  transp :: Int -> Int -> Int
  transp t i = (i + t) `mod` 12
  
 
-- | As 'intervalToPitch', but returns the 'Int' pitch class. 
intValToPitchClss :: Root -> Interval -> Int
intValToPitchClss r i = (toPitchClass r + toIntervalClss i) `mod` 12
                          
  
-- | The reverse of 'toPitchClass' returning the 'Note DiatonicNatural' given a 
-- Integer [0..11] semitone, where 0 represents C. When the integer is out 
-- of the range [0..11] an error is thrown.
pcToRoot :: Int -> Root
pcToRoot i 
  | 0 <= i && i <= 11 = roots !! i
  | otherwise         = error ("HarmTrace.Base.MusicRep.toRoot " ++
                               "invalid pitch class: " ++ show i)
                               
  
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
  
  
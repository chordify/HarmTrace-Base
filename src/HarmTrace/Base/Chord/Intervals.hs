{-# OPTIONS_GHC -Wall             #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Chord.Intervals
-- Copyright   :  (c) 2013 W. Bas de Haas and Jose Pedro Magalhaes
-- License     :  LGPL-3
--
-- Maintainer  :  bas@chordify.net, dreixel@chordify.net 
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: 
--------------------------------------------------------------------------------

module HarmTrace.Base.Chord.Intervals where

import HarmTrace.Base.Chord.Datatypes
import HarmTrace.Base.Chord.Internal

import Data.List                  ( partition )
import Data.IntSet                ( IntSet, fromList, union, insert, singleton
                                  , empty, (\\) )

-- | Converts an 'Int'erval class to an 'Interval'
icToInterval :: Int -> Interval
icToInterval i
  | 0 <= i && i <= 21 = intervals !! i
  | otherwise         = error ("HarmTrace.Base.MusicRep.toInterval " ++
                               "invalid pitch class: " ++ show i)

-- | Similar to 'toPitchClss', this function calculates an enharmonic 
-- interval class for each 'Note Interval' in the range of [0 .. 23]
-- ( == ['Note Nat I1' .. 'Note SS I13']
toIntervalClss :: Interval -> Int
toIntervalClss n@(Note m i) =
  --         1 2 3 4 5 6 7  8  9  10 11 12 13
  let ic = ([0,2,4,5,7,9,11,12,14,16,17,19,21] !! (fromEnum i)) + modToInt m 
  in  if ic >= 0 then ic
                 else error ("HarmTrace.Base.MusicRep.toIntervalClss: no "
                          ++ "interval class for " ++ show n)


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
     
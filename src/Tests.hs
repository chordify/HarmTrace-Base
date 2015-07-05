{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Chord.Tests
-- Copyright   :  (c) 2013--2014 W. Bas de Haas and Jose Pedro Magalhaes,
--                Multiphonyx Holding BV
-- License     :  LGPL-3
--
-- Maintainer  :  bas@chordify.net, dreixel@chordify.net 
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Defines some property tests for testing the HarmTrace.Base package
--------------------------------------------------------------------------------
module Main where

import HarmTrace.Base.Chord
import HarmTrace.Base.Parse   ( parseDataSafe, pChord )
import HarmTrace.Base.Time

import Test.QuickCheck
import Test.QuickCheck.Test   ( isSuccess )

import Data.List              ( sort, foldl' )

import System.Exit            ( exitFailure, exitSuccess )
import Control.Monad          ( when )

instance Arbitrary DiatonicNatural where
  arbitrary = elements . enumFrom $ C

instance Arbitrary IntNat where
  arbitrary = elements . enumFrom $ I1
  
instance Arbitrary Accidental where
  arbitrary = elements [Nat,Sh,Fl,SS,FF]


instance Arbitrary Root where
  arbitrary = elements . map pcToRoot $ [0..11]
  
instance Arbitrary Interval where
  arbitrary = choose (0,21) >>= return . icToInterval  
  
-- instance Arbitrary a => Arbitrary (Note a) where
  -- arbitrary = do nat <- arbitrary
                 -- acc <- arbitrary
                 -- return (Note acc nat)

               
instance Arbitrary Shorthand where
  arbitrary = elements . enumFrom $ Maj
  
instance Arbitrary Addition where
  arbitrary = oneof [ arbitrary >>= return . Add
                    , arbitrary >>= return . NoAdd ]

instance Arbitrary a => Arbitrary (Chord a) where
  arbitrary = do r   <- arbitrary
                 sh  <- elements [Maj, Min, Aug, Dim, Maj7, Min7, Sev, Dim7, HDim7, Aug7, MinMaj7]
                 -- sh  <- arbitrary
                 -- add <- arbitrary >>= listOf . return . Add 
                 
                 b   <- arbitrary
                 return (Chord r sh [] b ) -- (Note Nat I1))
                 
instance Arbitrary a => Arbitrary (Timed a) where
  arbitrary = do x  <- arbitrary
                 s  <- elements [2 .. 5]
                 ts <- vector s -- guarantee that this list has a minimum of 2 items
                 return . Timed x . sort $ ts
               
data ChkTimed = ChkTimed MeterKind [Timed ChordLabel] deriving (Show, Eq)

instance Arbitrary ChkTimed where
  arbitrary = do let -- Step function for creating a Timed ChordLabel
                     f :: [Timed a] -> (a, [NumData]) -> [Timed a]
                     f _     (_,[]) = error "should not happen" 
                     f [   ] (a, x) = [Timed a (map Time (0:x))]
                     f (h:t) (a, x) = let o   = offset h 
                                          g y = Time (y + o)
                                      in Timed a (map g (0:x)) : h : t
                     
                     -- creates additional duplicates at random places
                     -- dups ds ["a","b"] might return ["a", "a", "b"]
                     dups :: [Bool] -> [a] -> [a]
                     dups [ ]     l     = l
                     dups _      [ ]    = []
                     dups (b:bs) (e:es) | b         = e : dups bs (e : es)
                                        | otherwise = e : dups bs es
                 
                 ds <- arbitrary  -- [Bool]
                 as <- arbitrary >>= return . dups ds -- chords including duplicates
                 ns <- arbitrary >>= return . map (sort . map abs)
                                 >>= return . filter (\x -> let l = length x in l > 0 && l <= 4) 
                                
                 
                 mk <- arbitrary
                 bt <- elements [One, Two, Three, Four]
                 
                 return . ChkTimed mk . updateBeats mk bt 
                        . reverse . foldl' f [] $ zip as ns
                 
instance Arbitrary BeatTime where
  arbitrary = do choose (0.0, 100.0) >>= return . Time

instance Arbitrary MeterKind where 
  arbitrary = elements [Duple, Triple]

instance Arbitrary Beat where
  arbitrary = elements [One, Two, Three, Four, NoBeat]
                 
pcProp :: Root -> Bool
pcProp r = (toPitchClass r) == toPitchClass (pcToRoot (toPitchClass r))

pcSetProp :: Chord Root -> Bool
pcSetProp c = c == toChord (chordRoot c) (toIntSet c) (Just $ chordBass c)

intervalProp :: Interval -> Bool
intervalProp i = i == icToInterval (toIntervalClss i)

-- intervalProp2 :: Int -> Bool
-- intervalProp2 i = i == toIntervalClss (icToInterval i)

enHarEqProp :: Root -> Bool
enHarEqProp a = a &== a

parseProp :: Chord Root -> Bool
parseProp c = parseDataSafe pChord (show c) == c

mergeTimedTest, mergeTimedTest2, mergeTimedTest3, mergeTimedTest4 :: ChkTimed -> Bool
mergeTimedTest (ChkTimed _ cs) = expandTimed (mergeTimed cs) == expandTimed cs
mergeTimedTest2 (ChkTimed _ cs) = expandTimed (expandTimed cs) == expandTimed cs
mergeTimedTest3 (ChkTimed _ cs) = mergeTimed (mergeTimed cs) == mergeTimed cs
mergeTimedTest4 (ChkTimed _ cs) = mergeTimed (expandTimed cs) == mergeTimed cs

meterKind1, meterKind2 :: ChkTimed -> Bool
meterKind1 (ChkTimed Duple  cs) = mergeTimed (setMeterKind Duple  cs) == mergeTimed cs
meterKind1 (ChkTimed Triple cs) = mergeTimed (setMeterKind Triple cs) == mergeTimed cs

meterKind2 (ChkTimed Duple  cs) = setMeterKind Duple  cs == expandTimed cs
meterKind2 (ChkTimed Triple cs) = setMeterKind Triple cs == expandTimed cs

-- meterKind1 (ChkTimed Duple  cs) = setMeterKind Duple  (setMeterKind Triple cs) == cs
-- meterKind1 (ChkTimed Triple cs) = setMeterKind Triple (setMeterKind Duple  cs) == cs

correctNextBeat :: ChkTimed -> Bool
correctNextBeat (ChkTimed mk cs) = and . map (correctBeatTimes mk . getTimeStamps) $ cs

correctBeatTimes :: MeterKind -> [BeatTime] -> Bool
correctBeatTimes _  [ ] = True
correctBeatTimes _  [_] = True
correctBeatTimes mk (a:b:tl) = beat b == nextBeat mk (beat a) && correctBeatTimes mk tl

correctNextBeatMK :: (MeterKind, ChkTimed) -> Bool
correctNextBeatMK (mk, ChkTimed _ cs) = correctNextBeat 
                                      . ChkTimed mk . mergeTimed . setMeterKind mk $ cs

--------------------------------------------------------------------------------
-- Execute the tests
--------------------------------------------------------------------------------

main :: IO ()
main = do let myTest :: Testable p => String -> [p] -> IO ()
              myTest s p = do putStrLn (" *** Testing HarmTrace-Base: "++ s ++": ... ") 
                              rs <- mapM verboseCheckResult p
                              when (not . and . map isSuccess $ rs) exitFailure
                              
          myTest "roots"        [ pcProp, enHarEqProp ]
          myTest "chords"       [ pcSetProp, parseProp ]
          myTest "intervals I"  [ intervalProp ]
          -- myTest "intervals II" [ intervalProp2 ]
          myTest "mergeTimed"   [ mergeTimedTest, mergeTimedTest2, mergeTimedTest3, mergeTimedTest4 ] 
          myTest "nextBeat"     [ correctNextBeat ] 
          myTest "meterKind"    [ meterKind1, meterKind2 ] 
          myTest "meterKind II" [ correctNextBeatMK ] 
          exitSuccess
          
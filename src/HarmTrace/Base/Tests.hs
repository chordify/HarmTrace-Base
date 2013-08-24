{-# LANGUAGE FlexibleInstances                #-}
module HarmTrace.Base.Tests where

import HarmTrace.Base.MusicRep
import HarmTrace.Base.Parsing
import HarmTrace.Base.ChordTokenizer
import Test.QuickCheck
import Data.List (nub)

instance Arbitrary DiatonicNatural where
  arbitrary = elements . enumFrom $ C

instance Arbitrary IntNat where
  arbitrary = elements . enumFrom $ I1
  
instance Arbitrary Accidental where
  arbitrary = elements [Nat,Sh,Fl,SS,FF]


instance Arbitrary Root where
  arbitrary = elements . map toRoot $ [0..11]
  
instance Arbitrary Interval where
  arbitrary = choose (0,21) >>= return . toInterval  
  
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
                 
pcProp :: Root -> Bool
pcProp r = (toPitchClass r) == toPitchClass (toRoot (toPitchClass r))

pcSetProp :: Chord Root -> Bool
pcSetProp c = c == toChord (chordRoot c) (toIntValList c) (Just $ chordBass c)

intervalProp :: Interval -> Bool
intervalProp i = i == toInterval (toIntervalClss i)

intervalProp2 :: Int -> Bool
intervalProp2 i = i == toIntervalClss (toInterval i)

enHarEqProp :: Root -> Bool
enHarEqProp a = a &== a

parseProp :: Chord Root -> Bool
parseProp c = parseDataSafe pChord (show c) == c

c :: Chord Root
c = Chord rb Min [Add (Note Sh I6)] (Note Nat I1)

cs :: Chord Root
cs = Chord rd HDim7 [] (Note Nat I1)

rd :: Root
rd = Note Nat D

rc :: Root
rc = Note Nat C

rb :: Root
rb= Note Nat B
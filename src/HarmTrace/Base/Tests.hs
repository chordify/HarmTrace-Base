{-# LANGUAGE FlexibleInstances                #-}
module HarmTrace.Base.Tests where

import HarmTrace.Base.MusicRep
import Test.QuickCheck

instance Arbitrary DiatonicNatural where
  arbitrary = elements . enumFrom $ C

instance Arbitrary IntNat where
  arbitrary = elements . enumFrom $ I1
  
instance Arbitrary Accidental where
  arbitrary = elements [Nat,Sh,Fl,SS,FF]


instance Arbitrary Root where
  arbitrary = elements . map toRoot $ [0..11]
  
instance Arbitrary Interval where
  arbitrary = do nat <- arbitrary
                 acc <- arbitrary
                 return (Note acc nat)
{-  

instance Arbitrary a => Arbitrary (Note a) where
  arbitrary = do nat <- arbitrary
                 acc <- arbitrary
                 return (Note acc nat)
-}                 
instance Arbitrary Shorthand where
  arbitrary = elements . enumFrom $ Maj
  
instance Arbitrary Addition where
  arbitrary = oneof [ arbitrary >>= return . Add
                    , arbitrary >>= return . NoAdd ]

instance Arbitrary a => Arbitrary (Chord a) where
  arbitrary = do r   <- arbitrary
                 sh  <- arbitrary
                 add <- listOf arbitrary
                 b   <- arbitrary
                 return (Chord r sh add b)
                 
pcProp :: Root -> Bool
pcProp r = (toPitchClass r) == toPitchClass (toRoot (toPitchClass r))

pcSetProp :: Chord Root -> Bool
pcSetProp c = c == toChord (toIntValList c) (chordRoot c)

c :: Chord Root
c = Chord r Maj9 [] (Note Sh I13)

r :: Root
r = Note Nat D
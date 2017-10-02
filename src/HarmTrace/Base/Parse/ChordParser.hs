{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE FlexibleContexts   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Parse.ChordParser
-- Copyright   :  (c) 2012--2016, Chordify BV
-- License     :  LGPL-3
--
-- Maintainer  :  haskelldevelopers@chordify.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Some general parsing utilities used for parsing textual chord
-- representations.
--------------------------------------------------------------------------------

module HarmTrace.Base.Parse.ChordParser (
  -- * Parsing (elements of) chords
    pChord
  , pShorthand
  , pRoot
  , pAdditions
  , pAddition
  , pKey
  , pBeat
  ) where

import HarmTrace.Base.Parse.General
import HarmTrace.Base.Chord
import HarmTrace.Base.Time

import Data.List                   ( sort )

--------------------------------------------------------------------------------
-- Parsing String of Musical Chords
--------------------------------------------------------------------------------

-- | Parses a 'ChordLabel' in Harte et al. syntax including possible additions,
-- and removal of chord additions. If a chord has no 'Shorthand', the 'Degree'
-- list (if any) is analysed and depending on the 'Triad' (if any) a
-- 'Maj', 'Min','Aug', or 'Dim' 'Shorthand' is stored. By default all the
-- duration stored in every 'Chord' is 1 (where the unit is application
-- depended, often these are beats, but they can also be eighth notes)
pChord :: Parser ChordLabel
{-# INLINE pChord #-}
pChord =     pChordLabel
         <|> (NoChord    <$ (pString "N"  <|> pString "&pause"))
         <|> (UndefChord <$ (pSym '*'     <|> pSym 'X'))
         <?> "Chord"

-- Parses a chord label
pChordLabel :: Parser ChordLabel
{-# INLINE pChordLabel #-}
pChordLabel = mkChord <$> pRoot <* (pSym ':' `opt` ':')
                      <*> pMaybe pShorthand
                      <*> (pAdditions `opt` [])
                      <*> pInversion where

  mkChord :: Root -> Maybe Shorthand -> [Addition] -> Either Interval Root 
          -> ChordLabel
  -- if there are no degrees and no shorthand, following Harte it
  -- should be labelled a Maj chord
  mkChord r Nothing [] b = Chord   r Maj             [] (toInversion r b)
  mkChord r Nothing  a b = toChord r (addToIntSet a)    (toInversion r b)
  mkChord r (Just s) a b = Chord   r s               a  (toInversion r b)

  toInversion :: Root -> Either Interval Root -> Interval
  toInversion _  (Left  iv) = iv
  toInversion ra (Right rb) = pitchToInterval ra rb
  

pInversion :: Parser (Either Interval Root)
pInversion =    Left  <$ pSym '/' <*> pIntNote
           <|>  Right <$ pSym '/' <*> pRoot
           <<|> pure (Left $ Note Nat I1)
           <?> "/Inversion"           
  
-- | parses a musical key description, e.g. @C:maj@, or @D:min@
pKey :: Parser Key
pKey = f <$> pRoot <* pSym ':' <*> pShorthand <?> "Key"
  where f r m | m == Maj = Key r MajMode
              | m == Min = Key r MinMode
              | otherwise = error ("Tokenizer: key must be Major or Minor, "
                          ++ "found: " ++ show m)

-- | Parses a shorthand following Harte et al. syntax, but also the 'Shorthand's
-- added to the Billboard dataset, e.g. @maj@, @min@, or @9@.
pShorthand :: Parser Shorthand
{-# INLINE pShorthand #-}
pShorthand =     Maj      <$ pString "maj"
             <|> Min      <$ pString "min"
             <|> Dim      <$ pString "dim"
             <|> Aug      <$ pString "aug"
             <|> Maj7     <$ pString "maj7"
             <|> Min7     <$ pString "min7"
             <|> Sev      <$ pString "7"
             <|> Dim7     <$ pString "dim7"
             <|> HDim7    <$ pString "hdim" <* opt (pSym '7') '7'
             <|> MinMaj7  <$ pString "minmaj7"
             <|> Aug7     <$ pString "aug7"
             <|> Maj6     <$ pString "maj6"
             <|> Maj6     <$ pString "6"
             <|> Min6     <$ pString "min6"
             <|> Nin      <$ pString "9"
             <|> Maj9     <$ pString "maj9"
             <|> Min9     <$ pString "min9"
             <|> Five     <$ pString "5"
             <|> Sus2     <$ pString "sus2"
             <|> Sus4     <$ pString "sus4"
             <|> SevSus4  <$ pString "7sus4"
             -- additional Billboard shorthands
             <|> Min11    <$ pString "min11"
             <|> Min13    <$ pString "min13"
             <|> Maj13    <$ pString "maj13"
             <|> Eleven   <$ pString "11"
             <|> Thirteen <$ pString "13"
             <|> None     <$ pString "1" -- no shorthand: used in billboard to
                                         -- denote a rootnote only
             <?> "Shorthand"

-- | Parses a list of 'Chord' 'Addition's within parenthesis
pAdditions :: Parser [Addition]
pAdditions = sort <$> pPacked (pSym '(') (pSym ')') ( pListSep (pSym ',') pAddition )
             <?> "Addition List"

-- | Parses the a 'Chord' 'Addition' (or the removal of a chord addition,
-- prefixed by  a @*@)
pAddition :: Parser Addition
pAddition = (Add   <$>             pIntNote)
        <|> (NoAdd <$> (pSym '*'*> pIntNote))
        <?> "Addition"

-- | Parses an 'Interval'
pIntNote :: Parser Interval
pIntNote = Note <$> pAccidental <*> pInterval

-- | Parses in 'Accidental'
pAccidental :: Parser Accidental
pAccidental =    Sh <$ pSym    's'
             <|> Sh <$ pSym    '#'
             <|> Fl <$ pSym    'b'
             <|> SS <$ pString "ss"
             <|> SS <$ pString "##"
             <|> FF <$ pString "bb"
             <|> pure Nat <?> "Accidental"

-- | Parses an 'Interval'
pInterval :: Parser IntNat
pInterval =  foldr (<|>) pFail opts <?> "Interval" where
  opts = [i <$ pString (show i) | i <- [minBound..] ]

-- | Parses a 'Root' 'Note', e.g. @A@, @Bb@, or @F#@.
pRoot :: Parser Root
{-# INLINE pRoot #-}
pRoot = (flip Note) <$> pDiaNat <*> pAccidental

-- | Parses a 'DiatonicNatural'.
pDiaNat :: Parser DiatonicNatural
{-# INLINE pDiaNat #-}
pDiaNat =    A  <$ pSym 'A'
         <|> B  <$ pSym 'B'
         <|> C  <$ pSym 'C'
         <|> D  <$ pSym 'D'
         <|> E  <$ pSym 'E'
         <|> F  <$ pSym 'F'
         <|> G  <$ pSym 'G'

-- | Parses a 'Beat'.
pBeat :: Parser Beat
pBeat =   One    <$ pSym '1'
      <|> Two    <$ pSym '2'
      <|> Three  <$ pSym '3'
      <|> Four   <$ pSym '4'
      <|> NoBeat <$ pSym 'x'
      <?> "Beat"

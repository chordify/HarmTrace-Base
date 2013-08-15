{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE FlexibleContexts   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Parsing
-- Copyright   :  (c) 2012--2013 W. Bas de Haas and Jose Pedro Magalhaes
-- License     :  LGPL-3
--
-- Maintainer  :  bas@chordify.net, dreixel@chordify.net 
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Some general parsing utilities used for parsing textual chord
-- representations.
--------------------------------------------------------------------------------

module HarmTrace.Base.ChordTokenizer ( -- * Top level parser
                                       -- parseChordSeq 
                                       -- * Parsing (elements of) chords
                                       pChord
                                     , pShorthand
                                     , pRoot
                                     , pAdditions
                                     , pAddition
                                     , pKey
                                     ) where

import HarmTrace.Base.Parsing
import HarmTrace.Base.MusicRep

--------------------------------------------------------------------------------
-- Top level Chord sequence parser
--------------------------------------------------------------------------------

-- | Top level parser that parsers a string into a 'PieceLabel' and a posibly
-- empty list of errors
{-
parseChordSeq :: String -> (PieceLabel, [Error LineColPos])
parseChordSeq = parseDataWithErrors pSongAbs

--------------------------------------------------------------------------------
-- Tokenizing: parsing strings into tokens
--------------------------------------------------------------------------------  

-- | Parser that parses a string of whitespace-separated 'Chord's, e.g.
-- @C:maj Bb:9(s11);1 E:min7;1 Eb:min7;1 Ab:7;1 D:min7;1 G:7(13);1 C:maj6(9);1@
-- The first 'Chord' must be the key of the piece, and the after each chord
-- the semicolumn and an Integer representing the duration of the chord must 
-- be presented
pSongAbs :: Parser PieceLabel -- PieceRelToken -- 
pSongAbs = PieceLabel <$> pKey <* pLineEnd 
                      <*> (setLoc 0 <$> pListSep_ng pLineEnd pChordDur )
                      <*  pList pLineEnd where
  setLoc :: Int -> [Chord a] -> [Chord a]  
  setLoc _  [] = []
  setLoc ix (Chord r c d _ l :cs) = (Chord r c d ix l) : setLoc (ix+1) cs                               

-- parses chords with a duration (separated by a ';')
pChordDur :: Parser ChordLabel
pChordDur = (,) <$> pChord <*> (pSym ';' *> pNaturalRaw) <?> "Chord;Int"
-}

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
-- TODO add support for inversion
pChordLabel :: Parser ChordLabel
{-# INLINE pChordLabel #-}
pChordLabel = toChord <$> pRoot <* (pSym ':' `opt` ':') 
                      <*> pMaybe pShorthand
                      <*> (pAdditions `opt` []) 
                      <*> pInversion where
  
  toChord :: Root -> Maybe Shorthand -> [Addition] -> Maybe (Note Interval)
          -> ChordLabel
  -- if there are no degrees and no shorthand, following Harte it 
  -- should be labelled a Maj chord
  toChord r Nothing [] b = Chord r Maj      []              (inversion b)
  toChord r Nothing  a b = Chord r (toSh a) (remTriadDeg a) (inversion b)
  toChord r (Just s) a b = Chord r s        a               (inversion b) 
  
  -- prepares an inversion, if any
  inversion :: Maybe (Note Interval) -> (Note Interval)
  inversion = maybe (Note Nat I1) id
  
  -- removes the third and the fifth from an interval list
  remTriadDeg :: [Addition] -> [Addition]
  remTriadDeg = filter (\(Add (Note _ i)) -> i /= I3 || i /= I5)

  -- Calculates a shorthand if none has been given, but we have a list of 
  -- intervals
  toSh :: [Addition] -> Shorthand
  toSh d = case analyseDegTriad (addToIntValList d) of
             MajTriad -> Maj 
             MinTriad -> Min 
             AugTriad -> Aug 
             DimTriad -> Dim 
             NoTriad  -> None


-- Parses an inversion, but inversionsion are ignored for now.
pInversion :: Parser (Maybe (Note Interval))
pInversion = pMaybe (pSym '/' *> pIntNote) <?> "/Inversion"
             
-- | parses a musical key description, e.g. @C:maj@, or @D:min@
pKey :: Parser Key        
pKey = f <$> pRoot <* pSym ':' <*> pShorthand <?> "Key"
  where f r m | m == Maj = Key r MajMode
              | m == Min = Key r MinMode
              | otherwise = error ("Tokenizer: key must be Major or Minor, "
                          ++ "found: " ++ show m)

-- | Parses a shorthand following Harte et al. syntax, but also the shorthands
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
pAdditions = pPacked (pSym '(') (pSym ')') ( pListSep (pSym ',') pAddition ) 
             <?> "Addition List"

-- | Parses the a 'Chord' 'Addition' (or the removal of a chord addition, 
-- prefixed by  a @*@)
pAddition :: Parser Addition
pAddition = (Add   <$>             pIntNote)
        <|> (NoAdd <$> (pSym '*'*> pIntNote))
        <?> "Addition"

pIntNote :: Parser (Note Interval)
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
pInterval :: Parser Interval
pInterval =  ((!!) [minBound..] ) . pred <$> pNaturalRaw <?> "Interval"

-- | Parses a 'Root' 'Note', e.g. @A@, @Bb@, or @F#@.
pRoot :: Parser Root
{-# INLINE pRoot #-}
pRoot = (flip Note) <$> pDiaNat <*> pAccidental

pDiaNat :: Parser DiatonicNatural
pDiaNat =    A  <$ pSym 'A'
         <|> B  <$ pSym 'B'
         <|> C  <$ pSym 'C'
         <|> D  <$ pSym 'D'
         <|> E  <$ pSym 'E'
         <|> F  <$ pSym 'F'
         <|> G  <$ pSym 'G'
{-        <|> Note Fl A <$ pString "Ab"
        <|> Note Fl B <$ pString "Bb"
        <|> Note Fl C <$ pString "Cb"
        <|> Note Fl D <$ pString "Db"
        <|> Note Fl E <$ pString "Eb"
        <|> Note Fl F <$ pString "Fb"
        <|> Note Fl G <$ pString "Gb"
        <|> Note Sh A <$ pString "A#"
        <|> Note Sh B <$ pString "B#"
        <|> Note Sh C <$ pString "C#"
        <|> Note Sh D <$ pString "D#"
        <|> Note Sh E <$ pString "E#"
        <|> Note Sh F <$ pString "F#"
        <|> Note Sh G <$ pString "G#" <?> "Chord root"
-}
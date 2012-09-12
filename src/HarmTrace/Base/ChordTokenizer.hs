{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE FlexibleContexts   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Parsing
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Some general parsing utilities used for parsing textual chord
-- representations.
--------------------------------------------------------------------------------

module HarmTrace.Base.ChordTokenizer ( pChord, pShorthand, parseChordSeq
                                     , pSongAbs, pRoot
                                     , pDegrees, pDegree
                                     , pDeprRoot, pKey
                                     ) where

import HarmTrace.Base.Parsing
import HarmTrace.Base.MusicRep

--------------------------------------------------------------------------------
-- Top level Chord sequence parser
--------------------------------------------------------------------------------

parseChordSeq :: String -> (PieceLabel, [Error LineColPos])
parseChordSeq = parseDataWithErrors pSongAbs

--------------------------------------------------------------------------------
-- Tokenizing: parsing strings into tokens
--------------------------------------------------------------------------------  

-- Input is a string of whitespace-separated chords, e.g.
-- @Bb:9(s11) E:min7 Eb:min7 Ab:7 D:min7 G:7(13) C:maj6(9)@
-- First token is the key of the piece
pSongAbs :: Parser PieceLabel -- PieceRelToken -- 
pSongAbs = PieceLabel <$> pKey <* pLineEnd 
                      <*> (setLoc 0 <$> pListSep_ng pLineEnd pChordDur )
                      <*  pList pLineEnd where
  setLoc :: Int -> [Chord a] -> [Chord a]  
  setLoc _  [] = []
  setLoc ix (Chord r c d _ l :cs) = (Chord r c d ix l) : setLoc (ix+1) cs                               

-- parses chords with a duration (separated by a ';')
pChordDur :: Parser ChordLabel
pChordDur = setDur <$> pChord <*> (pSym ';' *> pNaturalRaw) where
  setDur c d = c {duration = d}

-- | Parses a 'ChordLabel' in Harte et al. syntax including possible additions, 
-- and removal of chord additions. If a chord has no 'Shorthand', the 'Degree' 
-- list (if any) is analysed and depending on the 'Triad' (if any) a 
-- 'Maj', 'Min','Aug', or 'Dim' 'Shorthand' is stored.
pChord :: Parser ChordLabel 
pChord =     pChordLabel 
         <|> (noneLabel    <$ (pString "N" <|> pString "&pause"))
         <|> (unknownLabel <$ (pString "*" <|> pString "X"))

pDeprRoot :: Parser Root 
pDeprRoot =     pRoot
            <|> Note Nothing   N  <$ pSym 'N' -- for no chord   
                    
-- For now, I assume there is always a shorthand, and sometimes extra
-- degrees. I guess it might be the case that sometimes there is no shorthand,
-- but then there certainly are degrees. 
pChordLabel :: Parser ChordLabel
pChordLabel = toChord <$> pRoot <* pSym ':'  <*> pMaybe pShorthand
                      -- we ignore optional inversions for now
                      <*> ((pDegrees `opt` []) <* pInversion)
  where -- if there are no degrees and no shorthand 
        toChord :: Root -> Maybe Shorthand -> [Addition] -> ChordLabel
        toChord r Nothing     [] = Chord r None [] 0 1
        toChord r Nothing     d  = case analyseDegTriad d of
                                     MajTriad -> Chord r Maj (remTriadDeg d) 0 1
                                     MinTriad -> Chord r Min (remTriadDeg d) 0 1
                                     AugTriad -> Chord r Aug (remTriadDeg d) 0 1
                                     DimTriad -> Chord r Dim (remTriadDeg d) 0 1
                                     NoTriad  -> Chord r None d 0 1
        toChord r (Just s)    d  = Chord r s d 0 1
        
        -- removes the third and the fifth from a Addtion list
        remTriadDeg :: [Addition] -> [Addition]
        remTriadDeg = filter (\(Add (Note _ i)) -> i /= I3 || i /= I5)

pInversion :: Parser (Maybe (Note Interval))
pInversion = (Just <$> (pSym '/' *> (Note <$> pMaybe pAccidental <*> pInterval))
             ) `opt` Nothing
             
-- | parses a musical key description, e.g. @C:maj@, or @D:min@
pKey :: Parser Key        
pKey = f <$> pRoot <* pSym ':' <*> pShorthand
  where f r m | m == Maj = Key r MajMode
              | m == Min = Key r MinMode
              | otherwise = error ("Tokenizer: key must be Major or Minor, "
                          ++ "found: " ++ show m)

-- | Parses a shorthand following Harte et al. syntax, but also the shorthands
-- added to the Billboard dataset, e.g. @maj@, @min@, or @9@.
pShorthand :: Parser Shorthand
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
             <|> Maj6     <$ pString "maj6"
             <|> Maj6     <$ pString "6"
             <|> Min6     <$ pString "min6"
             <|> Nin      <$ pString "9"
             <|> Maj9     <$ pString "maj9"
             <|> Min9     <$ pString "min9"
             <|> Five     <$ pString "5" 
             <|> Sus2     <$ pString "sus2" 
             <|> Sus4     <$ pString "sus4" 
             -- additional Billboard shorthands
             <|> Min11    <$ pString "min11" 
             <|> Min13    <$ pString "min13" 
             <|> Maj13    <$ pString "maj13" 
             <|> Eleven   <$ pString "11" 
             <|> Thirteen <$ pString "13" 
             <|> None     <$ pString "1" -- no shorthand: used in billboard to 
                                         -- denote a rootnote only
             <?> "Shorthand"

-- We don't produce intervals for a shorthand. This could easily be added,
-- though.
pDegrees :: Parser [Addition]
pDegrees = pPacked (pSym '(') (pSym ')') ( pListSep (pSym ',') pDegree )

-- | Parses the a 'Chord' 'Addition' (or the removal of a chord addition, 
-- prefixed by  a @*@)
pDegree :: Parser Addition
pDegree =  (Add   <$>              (Note <$> pMaybe pAccidental <*> pInterval))
       <|> (NoAdd <$> (pSym '*' *> (Note <$> pMaybe pAccidental <*> pInterval)))

-- | Parses in 'Accidental'       
pAccidental :: Parser Accidental
pAccidental =    Sh <$ pSym    's'
             <|> Sh <$ pSym    '#'
             <|> Fl <$ pSym    'b'
             <|> SS <$ pString "ss"
             <|> FF <$ pString "bb" <?> "Accidental"

-- | Parses an 'Interval'
pInterval :: Parser Interval
pInterval =  ((!!) [minBound..] ) . pred <$> pNaturalRaw

-- | Parses a 'Root' 'Note', e.g. @A@, @Bb@, or @F#@.
pRoot :: Parser Root
pRoot =     Note Nothing   A  <$ pSym 'A'
        <|> Note Nothing   B  <$ pSym 'B'
        <|> Note Nothing   C  <$ pSym 'C'
        <|> Note Nothing   D  <$ pSym 'D'
        <|> Note Nothing   E  <$ pSym 'E'
        <|> Note Nothing   F  <$ pSym 'F'
        <|> Note Nothing   G  <$ pSym 'G'
        <|> Note (Just Fl) A <$ pString "Ab"
        <|> Note (Just Fl) B <$ pString "Bb"
        <|> Note (Just Fl) C <$ pString "Cb"
        <|> Note (Just Fl) D <$ pString "Db"
        <|> Note (Just Fl) E <$ pString "Eb"
        <|> Note (Just Fl) F <$ pString "Fb"
        <|> Note (Just Fl) G <$ pString "Gb"
        <|> Note (Just Sh) A <$ pString "A#"
        <|> Note (Just Sh) B <$ pString "B#"
        <|> Note (Just Sh) C <$ pString "C#"
        <|> Note (Just Sh) D <$ pString "D#"
        <|> Note (Just Sh) E <$ pString "E#"
        <|> Note (Just Sh) F <$ pString "F#"
        <|> Note (Just Sh) G <$ pString "G#" <?> "Chord root"

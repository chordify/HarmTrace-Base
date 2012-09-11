{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE FlexibleContexts         #-}

module HarmTrace.Base.ChordTokenizer ( pChord, pShorthand
                                     , pSongAbs, pRoot
                                     , pDegrees, pDegree
                                     , pDeprRoot
                                     ) where

import HarmTrace.Base.Parsing
import HarmTrace.Base.MusicRep
-- import HarmTrace.Models.ChordTokens

--------------------------------------------------------------------------------
-- Tokenizing: parsing strings into tokens
--------------------------------------------------------------------------------  


-- Input is a string of whitespace-separated chords, e.g.
-- Bb:9(s11) E:min7 Eb:min7 Ab:7 D:min7 G:7(13) C:maj6(9)
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

pChord :: Parser ChordLabel 
pChord =     pDepricatedParseChord 
         <|> (noneLabel    <$ (pString "N" <|> pString "&pause"))
         <|> (unknownLabel <$ (pString "*" <|> pString "X"))

-- but then there certainly are degrees.

pDepricatedParseChord :: Parser ChordLabel
pDepricatedParseChord = f <$> pDeprRoot <* pSym ':' <*> pMaybe pShorthand
                          <*> ((pDegrees `opt` [])  <*  pInversion)
  where f r (Just s)    [] = Chord r s [] 0 1
        -- if there are no degrees and no shorthand (should not occur)
        -- we make it a minor chord
        f r Nothing     [] = Chord r Maj [] 0 1
        -- in case of there is no short hand we analyse the degree list
        f r Nothing     d  = Chord r (analyseDegs d) d 0 1
        -- in case of a sus4/maj we also analyse the degree list
        f r (Just Sus4) d  = Chord r (analyseDegs d) d 0 1
        f r (Just Maj)  d  = Chord r (analyseDegs d) d 0 1
        -- if we have another short hand we ignore the degrees list
        f r (Just s)    d  = Chord r s d 0 1

        -- analyses a list of Degrees and assigns a shortHand i.e. Chord Class        
        analyseDegs :: [Addition] -> Shorthand        
        analyseDegs d 
          | (Add (Note (Just Fl) I3) ) `elem` d = Min
          | (Add (Note (Just Sh) I5) ) `elem` d = Sev
          | (Add (Note (Just Fl) I7) ) `elem` d = Sev
          | (Add (Note  Nothing  I7) ) `elem` d = Maj7
          | (Add (Note (Just Fl) I9) ) `elem` d = Sev
          | (Add (Note (Just Sh) I9) ) `elem` d = Sev
          | (Add (Note  Nothing  I11)) `elem` d = Sev
          | (Add (Note (Just Sh) I11)) `elem` d = Sev
          | (Add (Note (Just Fl) I13)) `elem` d = Sev
          | (Add (Note  Nothing  I13)) `elem` d = Sev
          | (Add (Note  Nothing  I3) ) `elem` d = Maj
          | otherwise                     = Maj

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
pInversion = (Just <$> (pSym '/' *> (Note <$> pMaybe pModifier <*> pInterval))
             ) `opt` Nothing
             
-- parses a musical key description
pKey :: Parser Key        
pKey = f <$> pRoot <* pSym ':' <*> pShorthand
  where f r m | m == Maj = Key r MajMode
              | m == Min = Key r MinMode
              | otherwise = error ("Tokenizer: key must be Major or Minor, "
                          ++ "found: " ++ show m)

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

-- TODO removing degrees is not implemented 
pDegree :: Parser Addition
pDegree =   (Add   <$>              (Note <$> pMaybe pModifier <*> pInterval))
        <|> (NoAdd <$> (pSym '*' *> (Note <$> pMaybe pModifier <*> pInterval)))
              
pModifier :: Parser Modifier
pModifier =     Sh <$ pSym    's'
            <|> Sh <$ pSym    '#'
            <|> Fl <$ pSym    'b'
            <|> SS <$ pString "ss"
            <|> FF <$ pString "bb" <?> "Modifier"

pInterval :: Parser Interval
pInterval =  ((!!) [minBound..] ) . pred <$> pNaturalRaw

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

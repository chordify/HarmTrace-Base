{-# OPTIONS_GHC -Wall         #-}
{-# LANGUAGE FlexibleContexts #-}

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


module HarmTrace.Base.Parsing ( -- * Top level parsers 
                                parseData
                              , parseDataWithErrors
                                -- * Some general parsers
                              , pString
                              , pLineEnd
                              , pManyTill
                                -- * reexporting the uu-parsinglib
                              , module Data.ListLike.Base
                              , module Text.ParserCombinators.UU 
                              , module Text.ParserCombinators.UU.Utils
                              , module Text.ParserCombinators.UU.BasicInstances
                              ) where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils hiding (pSpaces)
import Text.ParserCombinators.UU.BasicInstances hiding (IsLocationUpdatedBy)
import Data.ListLike.Base (ListLike)

--------------------------------------------------------------------------------
-- A collection of parsing functions used by parsers throughout the project
--------------------------------------------------------------------------------     

-- | Toplevel parser that ignores error-reporting, regardless of there were
-- error in the parse
parseData :: (ListLike s a, Show a) => P (Str a s LineColPos) b -> s -> b
parseData p inp = fst ( parseDataWithErrors p inp )

-- | Toplevel parser that returns both the result as well as a (possibly empty)
-- list of error-corrections.
parseDataWithErrors :: (ListLike s a, Show a) 
                    =>  P (Str a s LineColPos) b -> s -> (b, [Error LineColPos])
parseDataWithErrors p inp = (parse ( (,) <$> p <*> pEnd) 
                             (createStr (LineColPos 0 0 0) inp))
                                                 
-- | Parses a specific string
pString :: (ListLike state a, IsLocationUpdatedBy loc a, Show a, Eq a) 
        => [a] -> P (Str a state loc) [a]
pString s = foldr (\a b -> (:) <$> a <*> b) (pure []) (map pSym s)

-- | parses UNIX and DOS/WINDOWS line endings inclucing trailing whitespace
pLineEnd :: Parser String
pLineEnd  = pString "\n" <|> pString "\r\n" <|> pString " " <|> pString "\t"  

-- | Parses an arbitrary times the first parsing combinator until the parsing 
-- second parsing combinator is encountered. The result of the second parsing
-- combinator is ignored.
pManyTill :: P st a -> P st b -> P st [a]
pManyTill p end = [] <$ end 
                  <<|> 
                  (:) <$> p <*> pManyTill p end


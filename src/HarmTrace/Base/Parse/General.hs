{-# OPTIONS_GHC -Wall         #-}
{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Parse.General
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


module HarmTrace.Base.Parse.General ( -- * Top level parsers
    parseData
  , parseDataWithErrors
  , parseDataSafe
    -- * Some general parsers
  , pString
  , pLineEnd
  , pManyTill
    -- Re-exporting the uu-parsinglib
  , module Text.ParserCombinators.UU
  , module Text.ParserCombinators.UU.Utils
  , module Text.ParserCombinators.UU.BasicInstances
  -- , module Data.ListLike.Base
  ) where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils          hiding ( pSpaces )
import Text.ParserCombinators.UU.BasicInstances hiding ( IsLocationUpdatedBy )
import Data.ListLike.Base                              ( ListLike )
import Data.List                                       ( intersperse )

--------------------------------------------------------------------------------
-- A collection of parsing functions used by parsers throughout the project
--------------------------------------------------------------------------------

-- | This is identical to 'parseData' however it will throw an 'error' when
-- the the list with parsing errors is not empty. No, this will not make your
-- program more \safe\. However, in certain cases you really want to be sure
-- that parsing has finished without errors. In those cases you should use
-- 'parseDataSafe'.
parseDataSafe :: (ListLike s a, Show a, Show s) =>
                  P (Str a s LineColPos) b -> s -> b
parseDataSafe p inp = case parseDataWithErrors p inp of
                        (dat, [] ) -> dat
                        (_  , err) -> error ("HarmTrace.Base.Parsing: a parsing"
                            ++ " function did not finish without errors. While"
                            ++ " parsing the data starting with:\n"
                            ++ (take 80 $ show inp)
                            ++ "\nThe following errors were encountered:\n"
                            ++ (concat . intersperse "\n" . take 50
                                       . map show $ err))

-- | Top-level parser that ignores error-reporting, regardless of there were
-- error in the parse
parseData :: (ListLike s a, Show a) => P (Str a s LineColPos) b -> s -> b
parseData p inp = fst ( parseDataWithErrors p inp )

-- | Top-level parser that returns both the result as well as a (possibly empty)
-- list of error-corrections.
parseDataWithErrors :: (ListLike s a, Show a)
                    =>  P (Str a s LineColPos) b -> s -> (b, [Error LineColPos])
parseDataWithErrors p inp = (parse ( (,) <$> p <*> pEnd)
                             (createStr (LineColPos 0 0 0) inp))

-- | Parses a specific string
pString :: (ListLike state a, IsLocationUpdatedBy loc a, Show a, Eq a)
        => [a] -> P (Str a state loc) [a]
{-# INLINABLE  pString #-}
pString s = foldr (\a b -> (:) <$> a <*> b) (pure []) (map pSym s)

-- | Parses UNIX and DOS/WINDOWS line endings including trailing whitespace
pLineEnd :: Parser String
pLineEnd  = pString "\n" <|> pString "\r\n" <|> pString " " <|> pString "\t"

-- | Parses an arbitrary times the first parsing combinator until the parsing
-- second parsing combinator is encountered. The result of the second parsing
-- combinator is ignored.
pManyTill :: P st a -> P st b -> P st [a]
pManyTill p end = [] <$ end
                  <<|>
                  (:) <$> p <*> pManyTill p end

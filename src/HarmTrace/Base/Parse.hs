{-# OPTIONS_GHC -Wall             #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Parse
-- Copyright   :  (c) 2012--2016, Chordify BV
-- License     :  LGPL-3
--
-- Maintainer  :  haskelldevelopers@chordify.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: An interface to all the sub modules of HarmTrace.Base.Parse
--------------------------------------------------------------------------------

module HarmTrace.Base.Parse (
    module HarmTrace.Base.Parse.General
  , module HarmTrace.Base.Parse.ChordParser
  ) where

import HarmTrace.Base.Parse.General
import HarmTrace.Base.Parse.ChordParser

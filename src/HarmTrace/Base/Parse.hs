{-# OPTIONS_GHC -Wall             #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Base.Parse
-- Copyright   :  (c) 2013 W. Bas de Haas and Jose Pedro Magalhaes
-- License     :  LGPL-3
--
-- Maintainer  :  bas@chordify.net, dreixel@chordify.net 
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

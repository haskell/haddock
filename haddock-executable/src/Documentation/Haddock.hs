{-# LANGUAGE PackageImports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Documentation.Haddock
-- Copyright   :  (c) David Waern 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskellorg
-- Stability   :  experimental
-- Portability :  portable
--
-- The Haddock API: A rudimentory, highly experimental API exposing some of
-- the internals of Haddock. Don't expect it to be stable.
--
-- This module re-exports "Documentation.Haddock" from the haddock-api
-- package for compatibility. This module will be removed in haddock 2.15.
-----------------------------------------------------------------------------
module Documentation.Haddock ( module Documentation.Haddock ) where

import "haddock-api" Documentation.Haddock


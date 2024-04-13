{-# LANGUAGE CPP #-}

-- |
-- Module      :  Language.Haskell.Extract
-- Copyright   :  (c) 2010, Oscar Finnsson
-- License     :  BSD-3-Clause, see LICENSE
--
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides useful helper functions for extracting a module's
-- top-level declarations from within template Haskell.
--
-- @since 0.1.2
module Language.Haskell.Extract (
  extractDeclarations,
  functionExtractor,
  functionExtractorMap,
  makeTupleExp,
  locationModule,
) where

import Data.List (nub)

import Text.Regex.Posix ((=~))

import Language.Haskell.TH (Exp (..), Lit (..), Loc (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Quasi (..))

--------------------------------------------------------------------------------

-- | Extracts the names of the top-level declarations that are defined in the
-- module containing the splice that this function is called from.
--
-- === __Example__
--
-- Obtain a list of all top-level declarations with identifiers that end in
-- @"oo"@. The elements of the resulting list will be a pair with the identifier
-- of the matching top-level declaration along with the matching top-level
-- declaration.
--
-- @
-- foo :: 'String'
-- foo = "Hello World!"
--
-- boo :: 'Double'
-- boo = 100.0
--
-- matches :: ['String']
-- matches = $('extractDeclarations' "oo$")
-- @
--
-- The splice @$('extractDeclarations' "oo$")@ in the code above will yield the
-- Haskell expression:
--
-- @
-- matches :: ['String']
-- matches = ["foo", "boo"]
-- @
--
-- @since 0.3.0
extractDeclarations :: Quasi m => String -> m [String]
extractDeclarations pattern = TH.runQ do
  loc  <- TH.location
  file <- TH.runIO (readFile (loc_filename loc))
  return $ nub $ filter (=~pattern) $ map fst $ concat $ map lex $ lines file

-- | Extracts the names and top-level declarations that are defined in the
-- module containing the splice that this function is called from.
--
-- === __Example__
--
-- Obtain a list of all top-level declarations with identifiers that end in
-- @"oo"@. The elements of the resulting list will be a pair with the identifier
-- of the matching top-level declaration along with the matching top-level
-- declaration.
--
-- @
-- foo :: 'Double'
-- foo = 10.0
--
-- boo :: 'Double'
-- boo = 100.0
--
-- matches :: [('String', 'Double')]
-- matches = $('functionExtractor' "oo$")
-- @
--
-- The splice @$('functionExtractor' "oo$")@ in the code above will yield the
-- Haskell expression:
--
-- @
-- matches :: [('String', 'Double')]
-- matches = [("foo", foo), ("boo", boo)]
-- @
--
-- @since 0.1.2
functionExtractor :: Quasi m => String -> m Exp
functionExtractor pattern = do
  functions <- extractDeclarations pattern
  pure (ListE (map makePairs functions))
  where
    makePairs :: String -> Exp
    makePairs name = makeTupleExp (LitE (StringL name)) (VarE (TH.mkName name))

-- | Extract the names and functions from the module and apply a function to
-- every pair. It is very useful if the common denominator of the functions is
-- a type class.
--
-- === __Example__
--
-- @
-- example :: ['String']
-- example = do
--   let expect = ["45", "88.8", "\"hello world!\""]
--       actual = $('functionExtractorMap' "^tc" [|\n f -> 'show' f|] )
--   'Control.Exception.assert' (expect == actual) expect
--
-- tcInt :: 'Integer'
-- tcInt = 45
--
-- tcDouble :: 'Double'
-- tcDouble = 88.8
--
-- tcString :: 'String'
-- tcString = "hello world!"
-- @
--
-- @since 0.1.2
functionExtractorMap :: Quasi m => String -> m Exp -> m Exp
functionExtractorMap pattern funcName = do
  fns <- extractDeclarations pattern
  fn  <- funcName

  let makePair :: String -> Exp
      makePair name = fn `AppE` LitE (StringL name) `AppE` VarE (TH.mkName name)

  pure (ListE (map makePair fns))

-- | @('makeTupleExp' a b)@ will produce a Haskell expression that is a pair
-- (i.e. a tuple with two elements) containing the two given expressions @a@
-- and @b@.
--
-- @since 0.3.0
makeTupleExp :: Exp -> Exp -> Exp
#if MIN_VERSION_template_haskell(2,15,0)
-- For template-haskell versions 2.16.0.0 and later.
makeTupleExp a b = TupE [Just a, Just b]
#else
-- For template-haskell versions 2.15.0.0 and earlier.
makeTupleExp a b = TupE [a, b]
#endif
{-# INLINE CONLIKE makeTupleExp #-}

-- | Extract the name of the current module as a 'String'. For example, the
-- @$'locationModule'@ splice in the following module:
--
-- === __Example__
--
-- @
-- {-# LANGUAGE TemplateHaskell #-}
--
-- module Examples.Hello.World
--   ( thisModuleName
--   ) where
--
-- import Language.Haskell.Extract ('locationModule')
--
-- thisModuleName :: 'String'
-- thisModuleName = $('locationModule')
-- @
--
-- >>> thisModuleName
-- Examples.Hello.World
--
-- @since 0.1.2
locationModule :: Quasi m => m Exp
locationModule = TH.runQ do
  loc <- TH.location
  pure (LitE (StringL (loc_module loc)))
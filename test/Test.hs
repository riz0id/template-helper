{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.Extract (functionExtractor, locationModule)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (assertFailure, (@=?))

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup "test"
    [ testCase "function" do
        case $(functionExtractor "^main") of
          []         -> assertFailure ("No matches for the pattern: \"^main\"")
          result : _ -> "main" @=? fst result
    , testCase "module" do
        "Main" @=? $(locationModule)
    ]
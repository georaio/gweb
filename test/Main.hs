{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified App.Abutting.Test as Abutting
import qualified Golden.Test as Golden
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  goldens <- Golden.tests
  defaultMain . testGroup "ALL" $
    [ testGroup "GOLDEN" goldens,
      Abutting.tests
    ]

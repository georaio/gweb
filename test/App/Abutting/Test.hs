{-# LANGUAGE OverloadedStrings #-}

module App.Abutting.Test (tests) where

import qualified App.Abutting as SUT
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "App.Abutting"
    [ testCase "empty" $
        SUT.flatten (SUT.wrap "") @?= "",
      testCase "one line" $
        SUT.flatten (SUT.wrap "foo") @?= "foo\n",
      testCase "two lines #1" $
        SUT.flatten (SUT.wrap "foo\nbar")
          @?= "foo\nbar\n",
      testCase "two lines #2" $
        SUT.flatten
          (mconcat [SUT.wrap "foo", SUT.wrap "bar"])
          @?= "foo\nbar\n",
      testCase "two lines #3" $
        SUT.flatten
          (mconcat [SUT.wrap "foo\nbar\n"])
          @?= "foo\nbar\n\n",
      testCase "indent #1" $
        SUT.flatten
          (SUT.indent 4 (SUT.wrap "foo\nbar"))
          @?= "    foo\n    bar\n",
      testCase "indent #2" $
        SUT.flatten
          (SUT.wrap "foo" <> SUT.indent 4 (SUT.wrap "bar\nquux"))
          @?= "foo\n    bar\n    quux\n",
      testCase "abut #1" $
        SUT.flatten
          (SUT.wrap "foo" `SUT.abut` SUT.wrap "bar\nquux")
          @?= "foobar\nquux\n",
      testCase "abut #2" $
        SUT.flatten
          (SUT.wrap "foo" `SUT.abut` SUT.indent 4 (SUT.wrap "bar\nquux"))
          @?= "foobar\n    quux\n"
    ]

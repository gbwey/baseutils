module Main where

import Test.Tasty
import qualified TestExtra

main :: IO ()
main = do
  defaultMain $
    testGroup
      "alltests"
      [ TestExtra.suite
      ]

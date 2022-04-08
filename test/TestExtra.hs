-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}

module TestExtra where

import Test.Tasty
import Test.Tasty.HUnit

-- import BaseUtils.Extra

suite :: TestTree
suite =
  testGroup
    "TestExtra"
    [ testCase "noop" $ True @?= True
    ]

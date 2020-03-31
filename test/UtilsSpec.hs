module UtilsSpec (spec) where

import Utils
import Linear.V3
import Linear.Vector
import Linear.Metric
import Test.Hspec
import Control.Monad.State
import System.Random

spec::Spec
spec = do
    describe "shuffledIndices" $
        it "shoud update sheed value" $ do
            let doubleShuffle = do
                    ind1 <- shuffledIndices 10
                    ind2 <- shuffledIndices 10
                    return $ (ind1, ind2)
            let gen0 = mkStdGen 100
            let (ind1, ind2) = evalState doubleShuffle gen0
            (ind1 == ind2) `shouldBe` False
    
module MaterialsSpec (spec) where

import Materials
import Linear.V3
import Linear.Vector
import Linear.Metric
import Test.Hspec

spec::Spec
spec = do
    let vin = V3 1 (-1) 0
    let nvin = normalize vin
    let n = V3 0 1 0
    let n_over_n' = 1/1.5

    describe "refract" $
        it "returns a vector opposite to given n" $ do
            (refract nvin n n_over_n') `shouldSatisfy` (\(Just (V3 x y z))-> y<0)
    
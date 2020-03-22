module Materials
    where
        import Types
        import Utils
        import Linear.V3
        import Linear.Vector
        import Linear.Metric
        import Data.Maybe

        -- lambertian
        lambertian::Color->Material
        lambertian albedo = Material { scatter=imp_scatter }
            where
                imp_scatter _ (HitEvent _ point n _) = do    
                    v <- randomPointInUnitSphere
                    let v' = n + v
                    return $ (Just $ Ray point v', albedo)

        -- metal
        metal::Color->Float->Material
        metal albedo fuzziness = Material { scatter=imp_scatter }
            where
                imp_scatter (Ray _ vin) (HitEvent _ point n _) = do
                    let f = bound (0, 1) fuzziness
                    s <- randomPointInUnitSphere
                    let vn = abs $ dot vin n
                    let vout = vin + (2*vn)*^n + f*^s
                    return $ if (dot vout n) >= 0
                        then (Just $ Ray point vout, albedo)
                        else (Nothing, albedo)
        
        -- glass

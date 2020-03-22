module Materials
    where
        import Types
        import Utils
        import Linear.V3
        import Linear.Vector
        import Linear.Metric

        -- lambertian
        lambertian::Color->Material
        lambertian albedo = Material { scatter=imp_scatter }
            where
                imp_scatter _ (HitEvent _ point n _) = do    
                    v <- randomPointInUnitSphere
                    let dir = n + v
                    return $ (Ray point dir, albedo)

        -- metal
        metal::Color->Material
        metal albedo = Material { scatter=imp_scatter }
            where
                imp_scatter (Ray _ v) (HitEvent _ point n _) = do    
                    let vn = abs $ dot v n
                    let dir = v + (2*vn)*^n
                    return $ (Ray point dir, albedo)
        
        -- glass

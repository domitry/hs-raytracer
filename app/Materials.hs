module Materials
    where
        import Types
        import Utils
        import Linear.Metric

        -- lambertian
        lambertian::Color->Material
        lambertian albedo = Material { scatter=scatter_ }
            where
                scatter_ (HitEvent _ point normal _) = do    
                    v <- randomPointInUnitSphere
                    let dir = (normalize normal)  + v
                    return $ (Ray point dir, albedo)

        -- metal
        -- glass

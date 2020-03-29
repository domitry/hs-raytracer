module Materials
    where
        import Types
        import Utils
        import Linear.V3
        import Linear.Vector
        import Linear.Metric
        import Data.Maybe

        -- be careful that dot vin nv < 0 and nv is normalized
        reflect::Vf->Vf->Vf
        reflect vin n = let vn = abs $ dot vin n in vin + (2*vn)*^n               

        -- be careful that dot vin nv < 0 and nv is normalized
        refract::Vf->Vf->Float->Maybe Vf
        refract nvin nv n_over_n'
            | d > 0 = Just $ (n_over_n')*^hor + (sqrt d)*^vert 
            | otherwise = Nothing
            where
                dt = dot nvin nv
                hor = nvin - dt*^nv -- horizonal comp of vin (sign is the same as vin)
                vert = (-1) *^ nv -- vertical comp of vin (sign is the same as vin)
                d = 1 - (n_over_n' * n_over_n')*(1-dt*dt)

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
                    let vout = (reflect vin n) + f*^s
                    return $ if (dot vout n) >= 0
                        then (Just $ Ray point vout, albedo)
                        else (Nothing, albedo)
        
        -- glass
        dielectric::Float->Material
        dielectric ri = Material { scatter=imp_scatter }
            where
                schlick cosine ri = r0+(1-r0)*((1-cosine)**5)
                    where
                        sqrt_r0 = (1-ri)/(1+ri)
                        r0 = sqrt_r0*sqrt_r0

                imp_scatter (Ray _ vin) (HitEvent _ point n _) = do
                    let nvin = normalize vin
                    let nvinn = dot nvin n
                    
                    let (normal, n_over_n', cosine) = if nvinn < 0
                        then (n, 1/ri, -nvinn)
                        else ((-1)*^n, ri, nvinn) -- the last element is ri*nvinnn in the book
                    
                    let reflected = reflect vin normal

                    prob <- randomRng (0, 1)
                    let prob_reflect = schlick cosine n_over_n'

                    let vout = case (refract nvin normal n_over_n') of
                            Nothing -> reflected
                            Just refracted -> if prob < prob_reflect then reflected else refracted
                    
                    return (Just $ Ray point vout, V3 1 1 1)

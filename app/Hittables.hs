module Hittables where
    import Linear.Vector
    import Linear.V3
    import Linear.Metric
    import Types
    import Utils

    -- a hitted point is back to origin
    -- select a point which is the nearest to the origin of the ray
    sphere::Vf->Float->Hittable
    sphere vc r = Hittable { hit = hit_ }
        where
            hitParam (Ray va vb)
                | d < 0 = Nothing
                | otherwise = bound (1e-3,1e5) ((-b-sqrt(d))/2.0/a)
                where
                    a = dot vb vb
                    b = 2*dot vb (va-vc)
                    c = dot (va-vc) (va-vc) - r*r
                    d = b*b-4*a*c
            
            hit_ ray = do
                param <- hitParam ray
                let point = extend ray param
                let normal = (point - vc)^/r
                return $ HitEvent param point normal

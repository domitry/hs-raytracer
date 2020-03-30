module Hittables where
    import Linear.Vector
    import Linear.V3
    import Linear.Metric
    import Types
    import Utils

    getUV normal = (u, v)
        where
            (V3 x y z) = normal
            v = ((asin y)+(pi/2))/pi
            u = 1-((atan2 z x)+pi)/2/pi

    -- select a point which is the nearest to the origin of the ray
    sphere::Vf->Float->Material->Hittable
    sphere vc r mat = Hittable { hit = hit_ }
        where
            hitParam (tmin, tmax) (Ray va vb)
                | d < 0 = Nothing
                | small > tmin && small < tmax  = Just small
                | big   > tmin && big   < tmax  = Just big
                | otherwise = Nothing
                where
                    a = dot vb vb
                    b = 2*dot vb (va-vc)
                    c = dot (va-vc) (va-vc) - r*r
                    d = b*b-4*a*c
                    sqd = sqrt d
                    small = (-b-sqd)/2/a
                    big = (-b+sqd)/2/a

            hit_ ray = do
                param <- hitParam (1e-3,1e5) ray
                let point = extend ray param
                let normal = (point - vc)^/r
                let uv = getUV normal
                return $ HitEvent {evParam=param, evPoint=point, evNormal=normal, evUV=uv, evMat=mat}

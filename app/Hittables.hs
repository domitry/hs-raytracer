module Hittables where
    import Linear.Vector
    import Linear.V3
    import Linear.Metric
    import Types
    import Utils

    compareAlongAxis::Int->Hittable->Hittable->Ordering
    compareAlongAxis axis a b
        | amin < bmin = LT
        | otherwise = GT
        where
            getRng::Int->AABB->(Float, Float)
            getRng axis (AABB xrng yrng zrng)
                | axis == 0 = xrng
                | axis == 1 = yrng
                | axis == 2 = zrng
            (amin, amax) = (getRng axis) $ bounding_box a
            (bmin, bmax) = (getRng axis) $ bounding_box b

    -- BVH (bounding volume hierarchies) node
    bvh_node::[Hittable]->State StdGen Hittable
    bvh_node hittables = do
        axis <- randomRng (0, 2) -- x or y or z
        let compare = compareAlongAxis axis
        let sorted = sortBy compare hittables
        
        retrun $ Hittable { hit = imp_hit,  bounding_box = box } where

            imp_hit (Ray a b) = hoge


    world::[Hittable]->Hittable
    world hittables = Hittable { hit = imp_hit, bounding_box = box } where
        parent = bvh_node hittables
        box = bounding_box parent
        imp_hit = hit bvh_node

    sphere::Vf->Float->Material->Hittable
    sphere vc r mat = Hittable { hit = imp_hit, bounding_box = box}
        where
            -- select a point which is the nearest to the origin of the ray
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

            getUV normal = (u, v) where
                (V3 x y z) = normal
                v = ((asin y)+(pi/2))/pi
                u = 1-((atan2 z x)+pi)/2/pi

            imp_hit ray = do
                param <- hitParam (1e-3,1e5) ray
                let point = extend ray param
                let normal = (point - vc)^/r
                let uv = getUV normal
                return $ HitEvent {evParam=param, evPoint=point, evNormal=normal, evUV=uv, evMat=mat}

            box = AABB (cx-r, cx+r) (cy-r, cy+r) (cz-r, cz+r)  
                where
                    (V3 cx, cy, cz) = vc

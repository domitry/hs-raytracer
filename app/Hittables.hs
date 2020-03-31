module Hittables where
    import Linear.Vector
    import Linear.V3
    import Linear.Metric
    import Data.List
    import Data.Maybe
    import Data.Ord
    import System.Random
    import Control.Monad.State
    import Types
    import Utils

    compareAlongAxis::Int->Hittable->Hittable->Ordering
    compareAlongAxis axis a b
        | amin < bmin = LT
        | otherwise = GT
        where
            rng (AABB x y z) = [x, y, z]!!axis
            (amin, _) = rng $ bounding_box a
            (bmin, _) = rng $ bounding_box b
    
    genChildren::(Hittable->Hittable->Ordering)->[Hittable]->State StdGen (Hittable, Hittable)
    genChildren comp hs
        | n == 1 = return $ (hs!!0, hs!!0)
        | n == 2 = return $ (hs!!0, hs!!1)
        | n >= 3 = do
            let (lhalf, rhalf) = splitAt (n `div` 2) (sortBy comp hs)
            left <- bvh_node lhalf
            right <- bvh_node rhalf
            return $ (left, right)
        where
            n = length hs

    -- BVH (bounding volume hierarchies) node
    bvh_node::[Hittable]->State StdGen Hittable
    bvh_node hs = do
        axis <- randomRng (0, 2)
        (left, right) <- genChildren (compareAlongAxis axis) hs
        let box = unionBox (bounding_box left) (bounding_box right)
        return $ Hittable { hit = imp_hit (left, right),  bounding_box = box } 
        where
            imp_hit (left, right) trng ray
                | null events = Nothing
                | otherwise = Just $ minimumBy (comparing evParam) events
                where
                    events = catMaybes [hit h trng ray | h <- [left, right], 
                        hitBox trng (bounding_box h) ray]

    genWorld::[Hittable]->State StdGen Hittable
    genWorld = bvh_node

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

            imp_hit trng ray = do
                param <- hitParam trng ray
                let point = extend ray param
                let normal = (point - vc)^/r
                let uv = getUV normal
                return $ HitEvent {evParam=param, evPoint=point, evNormal=normal, evUV=uv, evMat=mat}

            box = AABB (cx-r, cx+r) (cy-r, cy+r) (cz-r, cz+r)  
                where
                    (V3 cx cy cz) = vc

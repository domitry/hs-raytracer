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
            hitParam (tmin, tmax) (Ray _ va vb)
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

    xyplane::(Float,Float)->(Float,Float)->Float->Material->Hittable
    xyplane (x0,y0) (x1,y1) z mat = Hittable { hit=imp_hit, bounding_box=box } where
        box = AABB (x0,x1) (y0,y1) (z-0.1,z+0.1)
        imp_hit (tmin, tmax) (Ray _ va vb)
            | cx>x0 && cx<x1 && cy>y0 && cy<y1 && t>tmin && t<tmax = Just event
            | otherwise = Nothing
            where
                (V3 ax ay az) = va
                (V3 bx by bz) = vb
                t = (z-az)/bz
                point = va + t*^vb
                (V3 cx cy _) = point
                uv = (0, 0) -- TODO: implement getUV
                normal = V3 0 0 1
                event = HitEvent {evParam=t, evPoint=point, evNormal=normal, evUV=uv, evMat=mat}

    yzplane::(Float,Float)->(Float,Float)->Float->Material->Hittable
    yzplane (y0,z0) (y1,z1) x mat = Hittable { hit=imp_hit, bounding_box=box } where
        box = AABB (x-0.1,x+0.1) (y0,y1) (z0,z1) 
        imp_hit (tmin, tmax) (Ray _ va vb)
            | cy>y0 && cy<y1 && cz>z0 && cz<z1 && t>tmin && t<tmax = Just event
            | otherwise = Nothing
            where
                (V3 ax ay az) = va
                (V3 bx by bz) = vb
                t = (x-ax)/bx
                point = va + t*^vb
                (V3 _ cy cz) = point
                uv = (0, 0) -- TODO: implement getUV
                normal = V3 1 0 0
                event = HitEvent {evParam=t, evPoint=point, evNormal=normal, evUV=uv, evMat=mat}

    xzplane::(Float,Float)->(Float,Float)->Float->Material->Hittable
    xzplane (x0,z0) (x1,z1) y mat = Hittable { hit=imp_hit, bounding_box=box } where
        box = AABB (x0,x1) (y-0.1,y+0.1) (z0,z1)
        imp_hit (tmin, tmax) (Ray _ va vb)
            | cx>x0 && cx<x1 && cz>z0 && cz<z1 && t>tmin && t<tmax = Just event
            | otherwise = Nothing
            where
                (V3 ax ay az) = va
                (V3 bx by bz) = vb
                t = (y-ay)/by
                point = va + t*^vb
                (V3 cx _ cz) = point
                uv = (0, 0) -- TODO: implement getUV
                normal = V3 0 1 0
                event = HitEvent {evParam=t, evPoint=point, evNormal=normal, evUV=uv, evMat=mat}

    cube::Vf->Vf->Material->Hittable
    cube (V3 x0 y0 z0) (V3 x1 y1 z1) mat = Hittable { hit=imp_hit, bounding_box=box } where
        box = AABB (x0, x1) (y0, y1) (z0, z1)

        rgt = flipFace $ yzplane (y0,z0) (y1,z1) x0 mat
        lft = yzplane (y0,z0) (y1,z1) x1 mat
        btm = flipFace $ xzplane (x0,z0) (x1,z1) y0 mat
        top = xzplane (x0,z0) (x1,z1) y1 mat
        frt = flipFace $ xyplane (x0,y0) (x1,y1) z0 mat
        bck = xyplane (x0,y0) (x1,y1) z1 mat
        hittables = [rgt,lft,btm,top,frt,bck]
        
        imp_hit trng ray
            | null events = Nothing
            | otherwise = Just $ minimumBy (comparing evParam) events
            where
                events = catMaybes [hit h trng ray | h <- hittables]

    flipFace::Hittable->Hittable
    flipFace h = Hittable { hit=imp_hit, bounding_box=(bounding_box h) } where
        imp_hit trng ray = case (hit h trng ray) of
            Nothing -> Nothing
            Just event -> Just new_event where
                normal = (-1)*^(evNormal event)
                new_event = event { evNormal = normal }

    translate::Vf->Hittable->Hittable
    translate offset original = Hittable { hit=imp_hit, bounding_box=box } where
        (V3 dx dy dz) = offset
        AABB (x0, x1) (y0, y1) (z0, z1) = bounding_box original

        box = AABB (x0+dx,x1+dx) (y0+dy,y1+dy) (z0+dz,z1+dz)

        imp_hit trng (Ray time orig dir) = do
            let newray = Ray time (orig-offset) dir
            event <- hit original trng newray
            let point = evPoint event
            return $ event { evPoint = point+offset }

    rotateY::Float->Hittable->Hittable
    rotateY theta original = Hittable { hit=imp_hit, bounding_box=box } where
        (rotPos, rotNeg) = (rotY (st, ct), rotY ((-1)*st, ct)) where
            -- care that tt has opposite sign to theta
            (st, ct) = (sin tt, cos tt) where tt = (-pi)*(theta/180)
            rotY (st, ct) (V3 x y z) = V3 (ct*x-st*z) y (st*x+ct*z)

        box = AABB (x0',x1') yrng (z0',z1') where
            (AABB (x0, x1) yrng (z0, z1)) = bounding_box original
            corners = [(x,z) | x<-[x0,x1], z<-[z0,z1]]
            getXZ = \(V3 x y z)->(x, z)
            minXZ = \(x0,z0) (x1,z1)->(min x0 x1, min z0 z1)
            maxXZ = \(x0,z0) (x1,z1)->(max x0 x1, max z0 z1)
            (x0',z0') = foldl (\m (x,z) -> (minXZ m).getXZ.rotPos $ V3 x 0 z) (1e10,1e10) corners
            (x1',z1') = foldl (\m (x,z) -> (maxXZ m).getXZ.rotPos $ V3 x 0 z) ((-1e10),(-1e10)) corners
            
        imp_hit trng (Ray time orig dir) = do
            let newray = Ray time (rotNeg orig) (rotNeg dir)
            event <- hit original trng newray
            let pt = rotPos $ evPoint event
            let nv = rotPos $ evNormal event
            return $ event { evPoint=pt, evNormal=nv }

    addVelocity::Vf->Hittable->Hittable
    addVelocity vel original = Hittable { hit=imp_hit, bounding_box=box } where
        box = AABB xrng yrng zrng where
            (V3 dx dy dz) = vel
            AABB (x0, x1) (y0, y1) (z0, z1) = bounding_box original
            xs = [x0, x1, x0+dx, x1+dx]
            ys = [y0, y1, y0+dy, y1+dy]
            zs = [z0, z1, z0+dz, z1+dz]
            xrng = (minimum xs, maximum xs)
            yrng = (minimum ys, maximum ys)
            zrng = (minimum zs, maximum zs)

        imp_hit trng (Ray time orig dir) = do
            let offset = time*^vel
            let newray = Ray 0 (orig-offset) (dir-offset)
            event <- hit original trng newray
            let point = evPoint event
            return $ event { evPoint=(point+offset) }
    
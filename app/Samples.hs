module Samples where
    import Control.Monad.State
    import System.Random
    import Linear.Vector
    import Linear.V3
    import Linear.Metric
    import Utils
    import Types
    import Hittables (sphere, genWorld, xyplane)
    import Materials

    genLambertian::State StdGen Material
    genLambertian = do
        v1 <- randomV3 (0, 1)
        v2 <- randomV3 (0, 1)
        let albedo = v1*v2
        return $ lambertian $ fromColor albedo

    genMetal::State StdGen Material
    genMetal = do
        albedo <- randomV3 (0.5, 1)
        fuzz <- randomRng (0, 0.5)
        return $ metal albedo fuzz

    chooseMaterial::Float->State StdGen Material
    chooseMaterial prob
        | prob < 0.8  = genLambertian
        | prob < 0.95 = genMetal
        | otherwise = return $ dielectric 1.5

    genEarthScene::Image->State StdGen Scene
    genEarthScene img = do
        let green  = lambertian $ fromColor $ V3 0.8 0.8 0.0
        let big = sphere (V3 0.0 (-100.5) (-1.0)) 100.0 green
        let mat = lambertian $ fromImage img
        let earth = sphere (V3 0.0 0.0 (-1.0)) 0.5 mat  
        let mt = metal (V3 0.8 0.6 0.2) 0.0
        let left = sphere (V3 1 0 (-1)) 0.5 mt
        let glass = dielectric 1.5
        let right = sphere (V3 (-1) 0 (-1)) 0.5 glass
        world <- genWorld [big, earth, left, right]
        return $ Scene world cam_near_rear background_sky

    genColorfulScene::State StdGen Scene
    genColorfulScene = do
        let xzs = [(x, z) | z<-[-11..11], x<-[-11..11]]

        tmps <- forM xzs $ \(cx, cz) -> do
            dx <- randomRng (0, 0.9)
            dz <- randomRng (0, 0.9)
            return $ V3 (cx+dx) 0.2 (cz+dz)

        let center = V3 4 0.2 0
        let positions = [pos | pos <- tmps, (norm $ pos-center) > 0.9]

        shperes <- forM positions $ \pos -> do
            prob <- randomRng (0, 1)
            mat <- chooseMaterial prob
            return $ sphere pos 0.2 mat

        let ch = checker (V3 0.2 0.3 0.1) (V3 0.9 0.9 0.9)
        let ground = sphere (V3 0 (-1000) 0) 1000 (lambertian ch)
        let front = sphere (V3 4 1 0) 1 (metal (V3 0.7 0.6 0.5) 0.0)
        let center = sphere (V3 0 1 0) 1 (dielectric 1.5)
        let back = sphere (V3 (-4) 1 0) 1 (lambertian $ fromColor (V3 0.4 0.2 0.1))
        world <- genWorld $ [ground, front, center, back] ++ shperes
        return $ Scene world cam_last background_sky

    genBubbleScene::State StdGen Scene
    genBubbleScene = do
        let blue = lambertian $ fromColor $ V3 0.1 0.2 0.5
        let green  = lambertian $ fromColor $ V3 0.8 0.8 0.0
        let small = sphere (V3 0.0 0.0 (-1.0)) 0.5 blue
        let big = sphere (V3 0.0 (-100.5) (-1.0)) 100.0 green
        
        let mt = metal (V3 0.8 0.6 0.2) 0.0
        let left = sphere (V3 1 0 (-1)) 0.5 mt

        -- bubble
        let glass = dielectric 1.5
        let outer = sphere (V3 (-1) 0 (-1)) 0.5 glass
        let inner = sphere (V3 (-1) 0 (-1)) (-0.45) glass
        world <- genWorld [small, big, left, inner, outer] 
        return $ Scene world cam_front background_sky

    genMetalScene::State StdGen Scene
    genMetalScene = do
        let pink = lambertian $ fromColor $ V3 0.8 0.3 0.3
        let green  = lambertian $ fromColor $ V3 0.8 0.8 0.0
        let small = sphere (V3 0.0 0.0 (-1.0)) 0.5 pink
        let big = sphere (V3 0.0 (-100.5) (-1.0)) 100.0 green
        
        let mt1 = metal (V3 0.8 0.6 0.2) 1.0 -- super fuzzy
        let mt2 = metal (V3 0.8 0.8 0.8) 0.3 -- gray & a bit fuzzy
        let left = sphere (V3 1 0 (-1)) 0.5 mt1
        let right = sphere (V3 (-1) 0 (-1)) 0.5 mt2
        world <- genWorld [small, big, right, left]
        return $ Scene world cam_front background_sky

    genNoiseScene::State StdGen Scene
    genNoiseScene = do
        turb <- turbulence 4
        let mat = lambertian turb
        let small = sphere (V3 0 2 0) 2 mat
        let big = sphere (V3 0 (-1000) 0) 1000 mat
        world <- genWorld [small, big]
        return $ Scene world cam_last background_sky

    genMarbleScene::State StdGen Scene
    genMarbleScene = do
        marb <- marble 4
        let mat = lambertian marb
        let small = sphere (V3 0 2 0) 2 mat
        let big = sphere (V3 0 (-1000) 0) 1000 mat
        world <- genWorld [small, big]
        return $ Scene world cam_last background_sky

    genDarkMarbleScene::State StdGen Scene
    genDarkMarbleScene = do
        marb <- marble 4
        let dl = diffuseLight $ V3 4 4 4
        let mat = lambertian marb
        let small = sphere (V3 0 2 0) 2 mat
        let big = sphere (V3 0 (-1000) 0) 1000 mat
        let rect_light = xyplane (3,1) (5,3) (-2) dl
        let sphere_light = sphere (V3 0 7 0) 2 dl
        let cam = genCameraWithBokeh 10 0 2 20 (V3 26 3 6) (V3 0 2 0) (V3 0 1 0)

        world <- genWorld [small, big, rect_light, sphere_light]
        return $ Scene world cam background_night

    cam_bokeh::Camera
    -- focus_dist, aperture, asp, vfov, lookfrom, lookat, vup
    cam_bokeh = genCameraWithBokeh 3 1 2 45 (V3 (-2) 1 1) (V3 0 0 (-1)) (V3 0 1 0)

    cam_front::Camera
    -- asp, vfov, lookfrom, lookat, vup
    cam_front = genCamera 2 90 (V3 0 0 0) (V3 0 0 (-1)) (V3 0 1 0)

    cam_far_rear::Camera
    cam_far_rear = genCamera 2 90 (V3 (-2) 2 1) (V3 0 0 (-1)) (V3 0 1 0)

    cam_near_rear::Camera
    cam_near_rear = genCamera 2 45 (V3 (-2) 1 1) (V3 0 0 (-1)) (V3 0 1 0)

    cam_last::Camera
    cam_last = genCameraWithBokeh 10 0.1 2 20 (V3 13 2 3) (V3 0 0 0) (V3 0 1 0)

    background_sky::Ray->Color
    background_sky (Ray _ dir) = (1.0-t)*^(V3 1.0 1.0 1.0) + t*^(V3 0.5 0.7 1.0)
        where
            V3 x y z = normalize dir
            t = 0.5*(y+1.0)

    background_night::Ray->Color
    background_night _ = V3 0 0 0

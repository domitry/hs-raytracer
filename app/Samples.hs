module Samples where
    import Control.Monad.State
    import System.Random
    import Linear.Vector
    import Linear.V3
    import Linear.Metric
    import Utils
    import Types
    import Hittables
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
            vely <- if prob < 0.8 then randomRng (0, 0.5) else return 0
            return $ addVelocity (V3 0 vely 0) $ sphere pos 0.2 mat

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

    genCornellBox::State StdGen [Hittable]
    genCornellBox = do
        let red = lambertian $ fromColor $ V3 0.65 0.05 0.05
        let white = lambertian $ fromColor $ V3 0.73 0.73 0.73
        let green = lambertian $ fromColor $ V3 0.12 0.45 0.15
        let left = flipFace $ yzplane (0,0) (555,555) 555 green
        let right = yzplane (0,0) (555,555) 0 red
        let top = flipFace $ xzplane (0,0) (555,555) 555 white
        let bottom = xzplane (0,0) (555,555) 0 white
        let back = flipFace $ xyplane (0,0) (555,555) 555 white
        return $ [left,right,top,bottom,back]

    genCornellBoxScene::State StdGen Scene
    genCornellBoxScene = do
        let light = diffuseLight $ V3 15 15 15
        let rect_light = xzplane (213,227) (343,332) 554 light

        let white = lambertian $ fromColor $ V3 0.73 0.73 0.73
        let cube1 = cube (V3 0 0 0) (V3 165 330 165) white
        let cube2 = cube (V3 0 0 0) (V3 165 165 165) white
        let cube1'= translate (V3 265 0 295) $ rotateY 15 $ cube1
        let cube2'= translate (V3 130 0 65) $ rotateY (-18) $ cube2 

        let cam = genCameraWithBokeh 10 0 1 40 (V3 278 278 (-800)) (V3 278 278 0) (V3 0 1 0)

        parts <- genCornellBox
        world <- genWorld $ parts ++ [rect_light,cube1',cube2']
        return $ Scene world cam background_night

    genCornellBoxWithFogScene::State StdGen Scene
    genCornellBoxWithFogScene = do
        let light = diffuseLight $ V3 7 7 7
        let rect_light = xzplane (113,127) (443,432) 554 light
        let dammy = lambertian $ fromColor $ V3 0 0 0

        let cube1 = translate (V3 265 0 295) $ rotateY 15 $ box where
            box = cube (V3 0 0 0) (V3 165 330 165) dammy
        let cube2 = translate (V3 130 0 65) $ rotateY (-18) $ box where
            box = cube (V3 0 0 0) (V3 165 165 165) dammy

        let cube1' = constantMedium 0.01 cube1 black_fog where
            black_fog = isotropic $ V3 0 0 0
        let cube2' = constantMedium 0.01 cube2 white_fog where
            white_fog = isotropic $ V3 1 1 1

        let cam = genCameraWithBokeh 10 0 1 40 (V3 278 278 (-800)) (V3 278 278 0) (V3 0 1 0)
        parts <- genCornellBox
        world <- genWorld $ parts ++ [rect_light,cube1',cube2']
        return $ Scene world cam background_night

    genSecondFinalScene::Image->State StdGen Scene
    genSecondFinalScene img = do
        marble_sphere <- do
            marb <- marble 0.1
            let mat = lambertian marb
            return $ sphere (V3 220 280 300) 80 mat 

        let silver = sphere (V3 0 150 145) 50 mat where
            mat = metal (V3 0.8 0.8 0.9) 10

        let moving = addVelocity (V3 30 0 0) $ sphere (V3 400 400 200) 50 mat where
            mat = lambertian $ fromColor $ V3 0.7 0.3 0.1

        let earth = sphere (V3 400 200 400) 100 mat where
            mat = lambertian $ fromImage img

        let pure_glass = sphere (V3 260 150 45) 50 mat where
            mat = dielectric 1.5

        let blue_glass = sphere (V3 360 150 145) 70 mat where
            mat = dielectric 1.5

        let blue_fog = constantMedium 0.2 blue_glass mat where
            mat = isotropic $ V3 0.2 0.4 0.9

        let fog = constantMedium 1e-4 boundary mat where
            boundary = sphere (V3 0 0 0) 5000 (dielectric 1.5)
            mat = isotropic $ V3 1 1 1

        let light = xzplane (123,147) (423,412) 554 mat where
            mat = diffuseLight $ V3 7 7 7

        box_world <- do
            let green = lambertian $ fromColor $ V3 0.48 0.83 0.53
            let ijs = [(i, j) | i<-[0..19], j<-[0..19]]
            boxes <- forM ijs $ \(i, j)->do
                let w=100
                let (x0, z0) = (-1000+i*w, -100+j*w)
                let (x1, z1) = (x0+w, z0+w)
                y1 <- randomRng (1, 101)
                return $ cube (V3 x0 0 z0) (V3 x1 y1 z1) green
            genWorld boxes

        sphere_world <- do
            let white = lambertian $ fromColor $ V3 0.73 0.73 0.73
            spheres <- forM [1..1000] $ \_-> do
                center <- randomV3 (0, 165)
                return $ sphere center 10 white
            world <- genWorld spheres
            return $ translate (V3 (-100) 270 395) $ rotateY 15 world

        world <- genWorld [marble_sphere, silver, moving, earth, pure_glass, blue_glass,
                blue_fog, fog, light, box_world, sphere_world]

        let cam = genCameraWithBokeh 10 0 1 vfov lookfrom lookat vup where
            lookfrom = V3 478 278 (-600)
            lookat = V3 278 278 0
            vup = V3 0 1 0
            vfov = 40

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
    background_sky (Ray _ _ dir) = (1.0-t)*^(V3 1.0 1.0 1.0) + t*^(V3 0.5 0.7 1.0)
        where
            V3 x y z = normalize dir
            t = 0.5*(y+1.0)

    background_night::Ray->Color
    background_night _ = V3 0 0 0

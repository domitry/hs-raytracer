module Types where
    import Linear.V3
    import Linear.Vector
    import Linear.Metric
    import Control.Monad.State
    import System.Random

    type Color = V3 Float
    type Vf = V3 Float
    data Image = Image (Int, Int) [Color] deriving Show
    data Ray = Ray !Vf !Vf deriving Show -- orig, dir
    data Camera = Camera Vf Vf (Vf, Vf) (Vf, Vf) Float deriving Show -- orig, lower left corner, (hor, vert), (nhor, nvert), lens radius
    data HitEvent = HitEvent { evParam::Float, evPoint::Vf, evNormal::Vf, evUV::(Float,Float), evMat::Material }
    data Hittable = Hittable{ hit::Ray->Maybe HitEvent }
    data Material = Material{ scatter::Ray->HitEvent->State StdGen (Maybe Ray, Color) } -- ray_in, event -> (reflected, attenuation)
    data Texture = Texture{ pickColor::(Float, Float)->Vf->Color } -- (u, v)->point->col
    data World = World Hittable -- parent
    data AABB = AABB (Float, Float) (Float, Float) (Float, Float)
    
    -- optimized code by Andrew Kensler at Pixar
    hitBox::AABB->Ray->Bool
    hitBox (AABB (x0,x1) (y0,y1) (z0,z1)) (Ray (V3 ax ay az) (V3 bx by bz)) = tmax > tmin
            where
                xrng = sort [(x0-ax)/bx, (x1-ax)/bx]
                yrng = sort [(y0-ay)/by, (y1-ay)/by]
                zrng = sort [(z0-az)/bz, (z1-az)/bz]
                int (amin, amax) (bmin, bmax) = if tmax>tmin then (tmin, tmax) else (0, 0) where
                    (tmin, tmax) = (max amin bmin, min amax bmax) 
                (tmin, tmax) = int (int xrng yrng) (int xrng zrng)

    union::AABB->AABB->AABB
    union (AABB ax ay az) (AABB bx by bz) = AABB (uni ax bx) (uni ay by) (uni az bz)
        where uni (amin, amax) (bmin, bmax) = (min amin bmin, max amax bmax)

    extend::Ray->Float->Vf
    extend (Ray orig dir) t = orig + t*^dir

    genCameraWithBokeh::Float->Float->Float->Float->Vf->Vf->Vf->Camera
    genCameraWithBokeh focus_dist aperture aspect vfov lookfrom lookat vup =
        Camera orig llcorner (hor, vert) (nhor, nvert) lensr
            where
                theta = pi*(vfov/180)
                noc = normalize (lookat - lookfrom)
                nvert = vup - (dot vup noc)*^noc
                nhor = cross noc nvert
                halfh = focus_dist * tan (theta/2)
                halfw = aspect * halfh
                orig = lookfrom
                center = lookfrom + focus_dist *^ noc
                llcorner = center - halfh*^nvert - halfw*^nhor
                hor = (2*halfw)*^nhor
                vert = (2*halfh)*^nvert
                lensr = aperture/2

    genCamera::Float->Float->Vf->Vf->Vf->Camera
    genCamera = genCameraWithBokeh 1 0

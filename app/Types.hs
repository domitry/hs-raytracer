module Types where
    import Linear.V3
    import Linear.Vector
    import Linear.Metric
    import Control.Monad.State
    import System.Random
    import Data.List

    data Image = Image (Int, Int) [Color] deriving Show
    data Ray = Ray !Vf !Vf deriving Show -- orig, dir
    data Camera = Camera Vf Vf (Vf, Vf) (Vf, Vf) Float deriving Show -- orig, lower left corner, (hor, vert), (nhor, nvert), lens radius
    data HitEvent = HitEvent { evParam::Float, evPoint::Vf, evNormal::Vf, evUV::(Float,Float), evMat::Material }
    data Hittable = Hittable{ hit::(Float, Float)->Ray->Maybe HitEvent, bounding_box::AABB }
    data Material = Material{ scatter::Ray->HitEvent->State StdGen (Maybe Ray, Color) } -- ray_in, event -> (reflected, attenuation)
    data Texture = Texture{ pickColor::(Float, Float)->Vf->Color } -- (u, v)->point->col
    data AABB = AABB (Float, Float) (Float, Float) (Float, Float)
    type Color = V3 Float
    type Vf = V3 Float
    type World = Hittable
    
    -- "slab" algorithm
    -- optimized code by Andrew Kensler at Pixar
    hitBox::(Float, Float)->AABB->Ray->Bool
    hitBox (tmin, tmax) (AABB xrng yrng zrng) (Ray (V3 ax ay az) (V3 bx by bz)) = imax > imin
            where
                sortT (a, b) = if a<b then (a, b) else (b, a)
                -- get intersection between ray and two surfaces placed along each axis
                rng ((minv, maxv), (a, b)) = sortT ((minv-a)*invD, (maxv-a)*invD) where invD = 1/b
                ranges = map rng (zip [xrng, yrng, zrng] (zip [ax, ay, az] [bx, by, bz]))
                -- intersection of three ranges
                (imin, imax) = foldl (\(mmin, mmax) (cmin, cmax)-> (max mmin cmin, min mmax cmax)) (tmin, tmax) ranges

    unionBox::AABB->AABB->AABB
    unionBox (AABB ax ay az) (AABB bx by bz) = AABB (uni ax bx) (uni ay by) (uni az bz)
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

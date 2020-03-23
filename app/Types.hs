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
    data HitEvent = HitEvent Float Vf Vf Material -- param, point, normal
    data Hittable = Hittable{ hit::Ray->Maybe HitEvent }
    data Material = Material{ scatter::Ray->HitEvent->State StdGen (Maybe Ray, Color) } -- ray_in, event -> (reflected, attenuation)
    data World = World [Hittable]

    getParam::HitEvent->Float
    getParam (HitEvent param _ _ _) = param

    getMaterial::HitEvent->Material
    getMaterial (HitEvent _ _ _ mat) = mat

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

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
    data Camera = Camera Vf Vf Vf Vf deriving Show -- orig, lower left corner, hor, vert
    data HitEvent = HitEvent Float Vf Vf Material -- param, point, normal
    data Pixel = Pixel Float Float Float Float
    data Hittable = Hittable{ hit::Ray->Maybe HitEvent }
    data Material = Material{ scatter::HitEvent->State StdGen (Ray, Color) } -- point, normal -> (reflected, attenuation)
    data World = World [Hittable]

    getParam::HitEvent->Float
    getParam (HitEvent param _ _ _) = param

    getMaterial::HitEvent->Material
    getMaterial (HitEvent _ _ _ mat) = mat

    extend::Ray->Float->Vf
    extend (Ray orig dir) t = orig + t*^dir

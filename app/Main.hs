import Control.Monad
import Data.Char
import Data.Function
import Linear.Vector
import Linear.V3
import Linear.Metric

type Color = (Float, Float, Float)
type Vf = V3 Float
data Image = Image Int Int [Color] deriving Show
data Ray = Ray Vf Vf deriving Show -- orig, dir
data Camera = Camera Vf Vf Vf Vf deriving Show -- orig, lower left corner, hor, vert
data Sphere = Sphere Vf Float deriving Show -- cent, rad
data HitEvent = HitEvent Vf Vf deriving Show -- point, normal

extend::Ray->Float->Vf
extend (Ray orig dir) t = orig + t*^dir

hitSphereParam::Ray->Sphere->Maybe Float
hitSphereParam ray sphere 
    | t < 0 = Nothing
    | otherwise = Just ((-b-sqrt(t))/2.0/a) -- select a point which is the nearest to the origin of the ray
    where
        Ray va vb = ray
        Sphere vc r = sphere
        a = dot vb vb
        b = 2*dot vb (va-vc)
        c = dot (va-vc) (va-vc) - r*r
        t = b*b-4*a*c

hitSphere::Ray->Sphere->Maybe HitEvent
hitSphere ray sphere = case (hitSphereParam ray sphere) of
    Nothing -> Nothing
    Just param -> Just $ HitEvent point normal
        where
            Sphere center _ = sphere
            point = extend ray param
            normal = point - center

ray::Float->Float->Camera->Ray
ray u v cam = Ray orig dir
    where
        Camera orig llcorner hor vert = cam 
        dist = llcorner + (u *^ hor) + (v *^ vert)
        dir = dist - orig

background::Ray->Color
background ray = (r, g, b)
    where
        Ray orig (V3 x y z) = ray
        t = 0.5*(y+1.0) -- bound to [0,1]
        vec = (1.0-t)*^(V3 1.0 1.0 1.0) + t*^(V3 0.5 0.7 1.0)
        V3 r g b = vec

color::Ray->Color
color ray = case hitted of
    Nothing -> background ray
    Just (HitEvent _ normal) -> let V3 x y z = normalize normal in (0.5*(x+1.0), 0.5*(y+1.0), 0.5*(z+1.0))
    where
        sphere = Sphere (V3 0.0 0.0 (-1.0)) 0.5
        hitted = hitSphere ray sphere

render::Int->Int->Camera->Image
render nx ny cam = Image nx ny cols
    where
        xys = [(fromIntegral x, fromIntegral y) | y<-[0..(ny-1)], x<-[0..(nx-1)]]
        uvs = [(x/(fromIntegral nx),1.0-y/(fromIntegral ny)) | (x, y) <- xys]
        cols = map (\(u,v)->color $ ray u v cam) uvs

toPPM::Image->String
toPPM im = unlines(header ++ body)
    where
        Image nx ny cols = im
        header = ["P3",  unwords [show nx, show ny], "255"] -- magic, width, height, maxval
        body = [unwords $ map (\c -> show $ floor $ 255.9*c) [r,g,b] | (r,g,b) <- cols]

main = do
    let cam = Camera (V3 0.0 0.0 0.0) (V3 (-2.0) (-1.0) (-1.0)) (V3 4.0 0.0 0.0) (V3 0.0 2.0 0.0)     
    putStr $ toPPM $ render 400 200 cam

import System.Random
import Control.Monad
import Data.Char
import Data.Function
import Data.Maybe
import Data.List
import Data.Ord
import Linear.Vector
import Linear.V3
import Linear.Metric

type Color = (Float, Float, Float)
type Vf = V3 Float
data Image = Image Int Int [Color] deriving Show
data Ray = Ray Vf Vf deriving Show -- orig, dir
data Camera = Camera Vf Vf Vf Vf deriving Show -- orig, lower left corner, hor, vert
data HitEvent = HitEvent Float Vf Vf deriving Show -- param, point, normal
data Pixel = Pixel Float Float Float Float

-- solution from https://stackoverflow.com/questions/7787317/list-of-different-types
data Hittable = Hittable{ hit::Ray->Maybe HitEvent }
data World = World [Hittable]

hitSphere::Ray->Vf->Float->Maybe Float
hitSphere ray vc r
    | t < 0 = Nothing
    | (-b-sqrt(t))/2.0/a < 0 = Nothing -- a hitted point is back to origin 
    | otherwise = Just ((-b-sqrt(t))/2.0/a) -- select a point which is the nearest to the origin of the ray
    where
        Ray va vb = ray
        a = dot vb vb
        b = 2*dot vb (va-vc)
        c = dot (va-vc) (va-vc) - r*r
        t = b*b-4*a*c

sphere::Vf->Float->Hittable
sphere center rad = Hittable { hit = hit_}
    where
        hit_ ray = case (hitSphere ray center rad) of
            Nothing -> Nothing
            Just param -> Just $ HitEvent param point normal
                where
                    point = extend ray param
                    normal = point - center

hitWorld::Ray->World->Maybe HitEvent
hitWorld ray (World hittables) = case (events) of
    [] -> Nothing
    _ -> Just $ minimumBy (comparing (\(HitEvent param _ _)->param)) events
    where
        events = catMaybes [hit_(ray) | (Hittable hit_) <- hittables]

extend::Ray->Float->Vf
extend (Ray orig dir) t = orig + t*^dir

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

color::Ray->World->Color
color ray world = case (hitWorld ray world) of
    Nothing -> background ray
    Just (HitEvent _ _ normal) -> let V3 x y z = normalize normal in (0.5*(x+1.0), 0.5*(y+1.0), 0.5*(z+1.0))

mixColors::[Color]->Color
mixColors cols = (rt/nf, gt/nf, bt/nf)
    where
        total = foldl (\(r1,g1,b1,cnt)->
            (\(r2,g2,b2)->(r1+r2,g1+g2,b1+b2,cnt+1))) (0.0,0.0,0.0,0) cols
        (rt,gt,bt,n) = total
        nf = fromIntegral n

colorPixel::Pixel->Int->Camera->World->Color
colorPixel (Pixel u v uw uh) ns cam world = mixColors cols
    where
        rs = take (2*ns) $ randomRs (0.0,1.0) (mkStdGen 300)
        us = [u + r*uw | r <- take ns rs]
        vs = [v + r*uh | r <- drop ns rs]
        cols = map (\(u,v)->color (ray u v cam) world) $ zip us vs

render::Int->Int->Camera->World->Image
render nx ny cam world = Image nx ny cols
    where
        xys = [(fromIntegral x, fromIntegral y) | y<-[0..(ny-1)], x<-[0..(nx-1)]]
        uw = 1.0/(fromIntegral nx)
        uh = 1.0/(fromIntegral ny)
        uvs = [(x*uw,1.0-y*uh) | (x, y) <- xys]
        cols = [colorPixel (Pixel u v uw uh) 100 cam world | (u, v) <- uvs]

toPPM::Image->String
toPPM im = unlines(header ++ body)
    where
        Image nx ny cols = im
        header = ["P3",  unwords [show nx, show ny], "255"] -- magic, width, height, maxval
        body = [unwords $ map (\c -> show $ floor $ 255.9*c) [r,g,b] | (r,g,b) <- cols]

main = do
    let cam = Camera (V3 0.0 0.0 0.0) (V3 (-2.0) (-1.0) (-1.0)) (V3 4.0 0.0 0.0) (V3 0.0 2.0 0.0)
    let small = sphere (V3 0.0 0.0 (-1.0)) 0.5
    let big = sphere (V3 0.0 (-100.5) (-1.0)) 100.0
    let world = World [big, small] 
    putStr $ toPPM $ render 800 400 cam world

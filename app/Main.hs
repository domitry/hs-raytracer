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

type Color = V3 Float
type Vf = V3 Float
data Image = Image Int Int [Color] deriving Show
data Ray = Ray Vf Vf deriving Show -- orig, dir
data Camera = Camera Vf Vf Vf Vf deriving Show -- orig, lower left corner, hor, vert
data HitEvent = HitEvent Float Vf Vf deriving Show -- param, point, normal
data Pixel = Pixel Float Float Float Float
data Hittable = Hittable{ hit::Ray->Maybe HitEvent } --, event::Maybe Float->Maybe HitEvent }
data World = World [Hittable]

getParam::HitEvent->Float
getParam (HitEvent param _ _) = param

extend::Ray->Float->Vf
extend (Ray orig dir) t = orig + t*^dir

positiveOrNothing::Float->Maybe Float
positiveOrNothing n = if n >= 0 then Just n else Nothing

minimumByMaybe::(a -> a -> Ordering) -> [a] -> Maybe a
minimumByMaybe lmd lst = if null lst then Nothing else Just $ minimumBy lmd lst

average::[Vf] -> Vf
average vecs = V3 (rt/nf) (gt/nf) (bt/nf)
    where
        (rt,gt,bt,n) = foldl (\(r1,g1,b1,cnt)->
            (\(V3 r2 g2 b2)->(r1+r2,g1+g2,b1+b2,cnt+1))) (0.0,0.0,0.0,0) vecs
        nf = fromIntegral n

-- a hitted point is back to origin
-- select a point which is the nearest to the origin of the ray
sphere::Vf->Float->Hittable
sphere vc r = Hittable { hit = hit_ }
    where
        hitParam (Ray va vb)
            | t < 0 = Nothing
            | otherwise = positiveOrNothing ((-b-sqrt(t))/2.0/a)
            where
                a = dot vb vb
                b = 2*dot vb (va-vc)
                c = dot (va-vc) (va-vc) - r*r
                t = b*b-4*a*c
        
        hit_ ray = do
            param <- hitParam ray
            let point = extend ray param
            let normal = point - vc
            return $ HitEvent param point normal

hitWorld::Ray->World->Maybe HitEvent
hitWorld ray (World hitables) = minimumByMaybe (comparing getParam) events
    where
        events = catMaybes $ map (flip hit ray) hitables

genRay::Float->Float->Camera->Ray
genRay u v cam = Ray orig dir
    where
        Camera orig llcorner hor vert = cam 
        dist = llcorner + (u *^ hor) + (v *^ vert)
        dir = dist - orig

background::Ray->Color
background ray = (1.0-t)*^(V3 1.0 1.0 1.0) + t*^(V3 0.5 0.7 1.0)
    where
        Ray _ (V3 x y z) = ray
        t = 0.5*(y+1.0) -- bound to [0,1]

color::Ray->World->Color
color ray world = case (hitWorld ray world) of
    Nothing -> background ray
    Just (HitEvent _ _ normal) -> V3 (0.5*(x+1.0)) (0.5*(y+1.0)) (0.5*(z+1.0))
        where 
            V3 x y z = normalize normal

render::Int->Int->Camera->World->Image
render nx ny cam world = Image nx ny cols
    where
        ns = 100 -- number of sampling rays
        gen = mkStdGen 100
        genColor (x, y) = color (genRay u v cam) world
            where
                u = x/(fromIntegral nx)
                v = 1.0 - y/(fromIntegral ny)
        sample gen (cx, cy) = [(cx+dx, cy+dy) | (dx, dy) <- zip dxs dys]
            where
                (gen0, gen1) = split gen
                dxs = take ns $ randomRs (0.0, 1.0) gen0
                dys = take ns $ randomRs (0.0, 1.0) gen1
        xys = [(fromIntegral x, fromIntegral y) | y<-[0..(ny-1)], x<-[0..(nx-1)]]
        cols = map (\xy -> average $ map genColor (sample gen xy)) xys
        
toPPM::Image->String
toPPM im = unlines(header ++ body)
    where
        Image nx ny cols = im
        header = ["P3",  unwords [show nx, show ny], "255"] -- magic, width, height, maxval
        body = [unwords $ map (\c -> show $ floor $ 255.9*c) [r,g,b] | (V3 r g b) <- cols]

main = do
    let cam = Camera (V3 0.0 0.0 0.0) (V3 (-2.0) (-1.0) (-1.0)) (V3 4.0 0.0 0.0) (V3 0.0 2.0 0.0)
    let small = sphere (V3 0.0 0.0 (-1.0)) 0.5
    let big = sphere (V3 0.0 (-100.5) (-1.0)) 100.0
    let world = World [big, small] 
    putStr $ toPPM $ render 800 400 cam world

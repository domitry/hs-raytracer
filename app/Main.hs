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
import Control.Monad.State

type Color = V3 Float
type Vf = V3 Float
data Image = Image (Int, Int) [Color] deriving Show
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

bound::(Float, Float)->Float->Maybe Float
bound (tmin, tmax) t = if (t > tmin) && (t < tmax) then Just t else Nothing

minimumByMaybe::(a -> a -> Ordering) -> [a] -> Maybe a
minimumByMaybe lmd lst = if null lst then Nothing else Just $ minimumBy lmd lst

average::[Vf] -> Vf
average vecs = V3 (rt/nf) (gt/nf) (bt/nf)
    where
        (rt,gt,bt,n) = foldl (\(r1,g1,b1,cnt)->
            (\(V3 r2 g2 b2)->(r1+r2,g1+g2,b1+b2,cnt+1))) (0.0,0.0,0.0,0) vecs
        nf = fromIntegral n

randomRng::(Random a)=>(a, a)->State StdGen a
randomRng rng = state (\gen0 -> randomR rng gen0)

-- a hitted point is back to origin
-- select a point which is the nearest to the origin of the ray
sphere::Vf->Float->Hittable
sphere vc r = Hittable { hit = hit_ }
    where
        hitParam (Ray va vb)
            | d < 0 = Nothing
            | otherwise = bound (1e-3,1e5) ((-b-sqrt(d))/2.0/a)
            where
                a = dot vb vb
                b = 2*dot vb (va-vc)
                c = dot (va-vc) (va-vc) - r*r
                d = b*b-4*a*c
        
        hit_ ray = do
            param <- hitParam ray
            let point = extend ray param
            let normal = (point - vc)^/r
            return $ HitEvent param point normal

hitWorld::World->Ray->Maybe HitEvent
hitWorld (World hitables) ray = minimumByMaybe (comparing getParam) events
    where
        events = catMaybes $ map (flip hit ray) hitables

background::Ray->Color
background (Ray _ dir) = (1.0-t)*^(V3 1.0 1.0 1.0) + t*^(V3 0.5 0.7 1.0)
    where
        V3 x y z = normalize dir
        t = 0.5*(y+1.0)

randomPointInUnitSphere::State StdGen Vf
randomPointInUnitSphere = do
    x <- randomRng (0, 1)
    y <- randomRng (0, 1)
    z <- randomRng (0, 1)
    let vec = V3 x y z
    if norm vec <= 1 then return vec else randomPointInUnitSphere

reflect::HitEvent->State StdGen Ray
reflect (HitEvent _ point normal) = do    
    v <- randomPointInUnitSphere
    let dir = (normalize normal)  + v
    return $ Ray point dir

color::World->Ray->State StdGen Color
color world ray = case (hitWorld world ray) of
    Nothing -> return $ background ray
    Just event -> do
        reflected <- reflect event
        col <- color world reflected
        return $ 0.5 *^ col

sampleXYs::Int->(Float, Float)->State StdGen [(Float, Float)]
sampleXYs ns (cx, cy) = forM [1..ns] $ \_ -> do
    dx <- randomRng (0, 1)
    dy <- randomRng (0, 1)
    return (cx+dx, cy+dy)

toRay::Camera->(Float, Float)->Ray
toRay cam (u, v) = Ray orig dir
    where
        Camera orig llcorner hor vert = cam 
        dist = llcorner + (u *^ hor) + (v *^ vert)
        dir = dist - orig

render::(Int, Int)->Camera->World->State StdGen Image
render size cam world = do
    let (nx, ny) = size
    let xys = [(fromIntegral x, fromIntegral y) | y<-[0..(ny-1)], x<-[0..(nx-1)]]

    cols <- forM xys $ \cxy -> do 
        xys <- sampleXYs 100 cxy
        let toUV = \(x, y)->(x/(fromIntegral nx),  1.0 - y/(fromIntegral ny))
        let rays = map ((toRay cam).toUV) xys
        sampledCols <- forM rays (color world)
        return $ average sampledCols
    
    return $ Image size cols

toPPM::Image->String
toPPM im = unlines(header ++ body)
    where
        Image (nx, ny) cols = im
        header = ["P3",  unwords [show nx, show ny], "255"] -- magic, width, height, maxval
        body = [unwords $ map (\c -> show $ floor $ 255.9*c) [r,g,b] | (V3 r g b) <- cols]

main = do
    let cam = Camera (V3 0.0 0.0 0.0) (V3 (-2.0) (-1.0) (-1.0)) (V3 4.0 0.0 0.0) (V3 0.0 2.0 0.0)
    let small = sphere (V3 0.0 0.0 (-1.0)) 0.5
    let big = sphere (V3 0.0 (-100.5) (-1.0)) 100.0
    let world = World [small, big] 
    let size = (200, 100)
    stdgen <- getStdGen
    putStr $ toPPM $ evalState (render size cam world) stdgen

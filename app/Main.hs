import Control.Monad
import Control.Monad.State
import System.Random
import Data.Char
import Data.Function
import Data.Maybe
import Data.List
import Data.Ord
import Linear.Vector
import Linear.V3
import Linear.Metric
import Types
import Utils
import Hittables (sphere)
import Materials 

hitWorld::World->Ray->Maybe HitEvent
hitWorld (World hitables) ray = minimumByMaybe (comparing getParam) events
    where
        events = catMaybes $ map (flip hit ray) hitables

background::Ray->Color
background (Ray _ dir) = (1.0-t)*^(V3 1.0 1.0 1.0) + t*^(V3 0.5 0.7 1.0)
    where
        V3 x y z = normalize dir
        t = 0.5*(y+1.0)

color::World->Ray->State StdGen Color
color world ray = case (hitWorld world ray) of
    Nothing -> return $ background ray
    Just event -> do
        let (Material scatter_) = getMaterial event
        (reflected, attenuation) <- scatter_ event
        col <- color world reflected
        return $ attenuation * col

-- for anti-aliacing, sample ns points around the center of a pixel
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

main = do
    let cam = Camera (V3 0.0 0.0 0.0) (V3 (-2.0) (-1.0) (-1.0)) (V3 4.0 0.0 0.0) (V3 0.0 2.0 0.0)
    let gray = lambertian $ V3 0.5 0.5 0.5
    let small = sphere (V3 0.0 0.0 (-1.0)) 0.5 gray
    let big = sphere (V3 0.0 (-100.5) (-1.0)) 100.0 gray
    let world = World [small, big] 
    let size = (200, 100)
    stdgen <- getStdGen
    putStr $ toPPM $ evalState (render size cam world) stdgen

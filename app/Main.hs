import Control.Monad
import Control.Monad.State
import Control.Parallel.Strategies
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
import Materials (lambertian, metal)

hitWorld::World->Ray->Maybe HitEvent
hitWorld (World hitables) ray = minimumByMaybe (comparing getParam) events
    where
        events = catMaybes $ map (flip hit ray) hitables

background::Ray->Color
background (Ray _ dir) = (1.0-t)*^(V3 1.0 1.0 1.0) + t*^(V3 0.5 0.7 1.0)
    where
        V3 x y z = normalize dir
        t = 0.5*(y+1.0)

color::Int->World->Ray->State StdGen Color
color depth world ray
    | depth >= 50 = return black
    | otherwise = case (hitWorld world ray) of
        Nothing -> return $ background ray
        Just event -> do
            let genReflectedColor = \reflected->(color (depth+1) world reflected)
            let (Material scatter_) = getMaterial event
            (maybeReflected, attenuation) <- scatter_ ray event
            col <- maybe (return black) genReflectedColor maybeReflected
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

renderPixel::(Int, Int)->Camera->World->(Float, Float)->State StdGen Color
renderPixel (nx, ny) cam world cxy = do
    xys <- sampleXYs 100 cxy
    let toUV = \(x, y)->(x/(fromIntegral nx),  1 - y/(fromIntegral ny))
    let rays = map ((toRay cam).toUV) xys
    sampledCols <- forM rays (color 0 world)
    return $ average sampledCols

render::(Int, Int)->Camera->World->IO Image
render size cam world = do
    let (nx, ny) = size
    let xys = [(fromIntegral x, fromIntegral y) | y<-[0..(ny-1)], x<-[0..(nx-1)]]
    
    gens <- forM [1..(nx*ny)] $ \_ -> do
        newStdGen
        getStdGen
    
    let render_ = renderPixel size cam world
    let cols = parMap rpar (\(xy, g) -> evalState (render_ xy) g) (zip xys gens)
    return $ Image size cols

main = do
    let cam = Camera (V3 0.0 0.0 0.0) (V3 (-2.0) (-1.0) (-1.0)) (V3 4.0 0.0 0.0) (V3 0.0 2.0 0.0)
    let pink = lambertian $ V3 0.8 0.3 0.3
    let green  = lambertian $ V3 0.8 0.8 0.0
    let small = sphere (V3 0.0 0.0 (-1.0)) 0.5 pink
    let big = sphere (V3 0.0 (-100.5) (-1.0)) 100.0 green
    
    let mt1 = metal (V3 0.8 0.6 0.2) 1.0 -- super fuzzy
    let mt2 = metal (V3 0.8 0.8 0.8) 0.3 -- gray & a bit fuzzy
    let left = sphere (V3 1 0 (-1)) 0.5 mt1
    let right = sphere (V3 (-1) 0 (-1)) 0.5 mt2

    let world = World [small, big, left, right] 
    let size = (200, 100)
    
    img <- render size cam world
    putStr $ toPPM $ gammaCorrection img

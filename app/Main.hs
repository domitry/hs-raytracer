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
            let mat = getMaterial event
            (maybeReflected, attenuation) <- scatter mat ray event
            col <- maybe (return black) (color (depth+1) world) maybeReflected
            return $ attenuation * col

-- for anti-aliacing, sample ns points around the center of a pixel
sampleXYs::Int->(Float, Float)->State StdGen [(Float, Float)]
sampleXYs ns (cx, cy) = forM [1..ns] $ \_ -> do
    dx <- randomRng (0, 1)
    dy <- randomRng (0, 1)
    return (cx+dx, cy+dy)

toRay::Camera->(Float, Float)->State StdGen Ray
toRay cam (u, v) = do
    let Camera orig llcorner (hor, vert) (nhor, nvert) lensr = cam
    let dst = llcorner + (u *^ hor) + (v *^ vert)
    (dx, dy) <- randomPointInUnitCircle
    let offset = (dx*^nhor) + (dy*^nvert)
    let orig2 = orig + lensr*^offset
    let dir = dst - orig2
    return $ Ray orig2 dir

renderPixel::(Int, Int)->Camera->World->(Float, Float)->State StdGen Color
renderPixel (nx, ny) cam world cxy = do
    xys <- sampleXYs 100 cxy
    let toUV = \(x, y)->(x/(fromIntegral nx),  1 - y/(fromIntegral ny))
    rays <- forM xys ((toRay cam).toUV)
    sampledCols <- forM rays (color 0 world)
    return $ average sampledCols

render::(Int, Int)->Camera->World->IO Image
render size cam world = do
    let (nx, ny) = size
    let xys = [(fromIntegral x, fromIntegral y) | y<-[0..(ny-1)], x<-[0..(nx-1)]]
    gen0 <- getStdGen
    let render_ = renderPixel size cam world
    let cols = parMap rpar (\(xy, g) -> evalState (render_ xy) g) (zip xys (mkGens gen0))
    return $ Image size cols

main = do
    -- asp, vfov, lookfrom, lookat, vup
    let cam0 = genCamera 2 90 (V3 0 0 0) (V3 0 0 (-1)) (V3 0 1 0) -- front
    let cam1 = genCamera 2 90 (V3 (-2) 2 1) (V3 0 0 (-1)) (V3 0 1 0) -- rear
    let cam2 = genCamera 2 45 (V3 (-2) 1 1) (V3 0 0 (-1)) (V3 0 1 0) -- rear (near)
    let cam_bokeh = genCameraWithBokeh 3 1 2 45 (V3 (-2) 1 1) (V3 0 0 (-1)) (V3 0 1 0)
    
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
    
    img <- render size cam_bokeh world
    putStr $ toPPM $ gammaCorrection img

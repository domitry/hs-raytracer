import Control.Monad
import Control.Monad.State
import Control.Parallel.Strategies
import System.Random
import System.IO
import Data.Time
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
import Materials (lambertian, metal, dielectric, checker, perlin)
import Samples

background::Ray->Color
background (Ray _ dir) = (1.0-t)*^(V3 1.0 1.0 1.0) + t*^(V3 0.5 0.7 1.0)
    where
        V3 x y z = normalize dir
        t = 0.5*(y+1.0)

color::Int->World->Ray->State StdGen Color
color depth world ray
    | depth >= 50 = return black
    | otherwise = case (hit world (1e-3,1e5) ray) of
        Nothing -> return $ background ray
        Just event -> do
            let mat = evMat event
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
    gen0 <- getStdGen
    let cam = cam_last
    let world = evalState (genMarbleWorld) gen0
    newStdGen

    start <- getCurrentTime
    img <- render (400, 200) cam world
    putStr $ toPPM $ gammaCorrection img
    end <- getCurrentTime
    hPutStrLn stderr ("Rendering Time: " ++ (show $ diffUTCTime end start))

    putStr $ toPPM img

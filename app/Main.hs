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

-- this is not Int->Scene->Ray->... because scene is reused in the latter self-call
color::Int->World->Background->Ray->State StdGen Color
color depth world background ray
    | depth >= 50 = return black
    | otherwise = case (hit world (1e-3,1e20) ray) of
        Nothing -> return $ background ray
        Just event -> do
            let mat = evMat event
            let emitted = emit mat event
            (maybeNewRay, attenuation) <- scatter mat ray event
            col <- maybe (return emitted) (color (depth+1) world background) maybeNewRay
            return $ emitted + attenuation * col

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
    return $ Ray 0 orig2 dir

renderPixel::(Int, Int)->Scene->(Float, Float)->State StdGen Color
renderPixel (nx, ny) (Scene world cam background) cxy = do
    let nsample = 400
    xys <- sampleXYs nsample cxy
    let toUV = \(x, y)->(x/(fromIntegral nx),  1 - y/(fromIntegral ny))
    tmp_rays <- forM xys ((toRay cam).toUV)
    -- add randomized time for Motion Blur
    times <- forM [1..nsample] (\_->randomRng (0, 1))
    let rays = [Ray time orig dir | (time, (Ray _ orig dir)) <- zip times tmp_rays]
    sampledCols <- forM rays (color 0 world background)
    return $ average sampledCols

render::(Int, Int)->Scene->IO Image
render size scene = do
    let (nx, ny) = size
    let xys = [(fromIntegral x, fromIntegral y) | y<-[0..(ny-1)], x<-[0..(nx-1)]]
    gen0 <- getStdGen
    let render_ = renderPixel size scene
    let cols = parMap rpar (\(xy, g) -> evalState (render_ xy) g) (zip xys (mkGens gen0))
    return $ Image size cols

main = do
    gen0 <- getStdGen
    earth <- fromPPM "/home/nishida/hslearn/Raytracer/small.ppm"
    let scene = evalState (genSecondFinalScene earth) gen0
    newStdGen

    start <- getCurrentTime
    img <- render (400, 400) scene
    putStr $ toPPM $ gammaCorrection img
    end <- getCurrentTime
    hPutStrLn stderr ("Rendering Time: " ++ (show $ diffUTCTime end start))

    putStr $ toPPM img

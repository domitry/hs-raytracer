module Utils where
    import Control.Monad.State
    import Linear.Vector
    import Linear.V3
    import Linear.Metric
    import Data.Function
    import Data.List
    import System.Random
    import System.IO
    import Types

    black::Vf
    black = V3 0 0 0

    boundMaybe::(Float, Float)->Float->Maybe Float
    boundMaybe (tmin, tmax) t = if (t > tmin) && (t < tmax) then Just t else Nothing

    bound::(Float, Float)->Float->Float
    bound (tmin, tmax) t
        | t < tmin = tmin
        | t > tmax = tmax
        | otherwise = t

    minimumByMaybe::(a -> a -> Ordering) -> [a] -> Maybe a
    minimumByMaybe lmd lst = if null lst then Nothing else Just $ minimumBy lmd lst

    average::[Vf] -> Vf
    average vecs = V3 (rt/nf) (gt/nf) (bt/nf)
        where
            (rt,gt,bt,n) = foldl (\(r1,g1,b1,cnt)->
                (\(V3 r2 g2 b2)->(r1+r2,g1+g2,b1+b2,cnt+1))) (0.0,0.0,0.0,0) vecs
            nf = fromIntegral n

    mkGens::StdGen->[StdGen]
    mkGens gen = unfoldr (\gen-> Just $ split gen) gen

    randomRng::(Random a)=>(a, a)->State StdGen a
    randomRng rng = state (\gen0 -> randomR rng gen0)

    randomV3::(Random a)=>(a, a)->State StdGen (V3 a)
    randomV3 rng = do
        x <- randomRng rng
        y <- randomRng rng
        z <- randomRng rng
        return $ V3 x y z

    randomPointInUnitSphere::State StdGen Vf
    randomPointInUnitSphere = do
        x <- randomRng ((-1), 1)
        y <- randomRng ((-1), 1)
        z <- randomRng ((-1), 1)
        let vec = V3 x y z
        if norm vec <= 1 then return vec else randomPointInUnitSphere

    randomPointInUnitCircle::State StdGen (Float, Float)
    randomPointInUnitCircle = do
        x <- randomRng ((-1), 1)
        y <- randomRng ((-1), 1)
        if (x**2 + y**2) <= 1 then return (x, y) else randomPointInUnitCircle

    toPPM::Image->String
    toPPM im = unlines(header ++ body)
        where
            Image (nx, ny) cols = im
            header = ["P3",  unwords [show nx, show ny], "255"] -- magic, width, height, maxval
            body = [unwords $ map (\c -> show $ floor $ 255.9*c) [r,g,b] | (V3 r g b) <- cols]

    fromPPM::String->IO Image
    fromPPM fname = do
        str <- readFile fname
        let ls = [line | line <- lines str, head line /= '#']
        let wh = [read s | s <- words $ ls!!1]
        let maxCol = read $ ls!!2
        let (w, h) = (wh!!0, wh!!1)
        cols <- forM [0..(w*h-1)] $ \i -> do
            let j = 3+3*i
            let r = read $ ls!!(j)
            let g = read $ ls!!(j+1)
            let b = read $ ls!!(j+2)
            return $ V3 (r/maxCol) (g/maxCol) (b/maxCol)
        return $ Image (w, h) cols

    gammaCorrection::Image->Image
    gammaCorrection (Image size cols) = Image size $ map (\(V3 r g b)->V3 (r**0.5) (g**0.5) (b**0.5)) cols

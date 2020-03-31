module Utils where
    import Control.Monad.State
    import Linear.Vector
    import Linear.V3
    import Linear.Metric
    import Data.Function
    import Data.List
    import Data.Vector (fromList, (!), Vector)
    import System.Random
    import System.Random.Shuffle
    import System.IO
    import Types

    black::Vf
    black = V3 0 0 0

    bound::(Float, Float)->Float->Float
    bound (tmin, tmax) t
        | t < tmin = tmin
        | t > tmax = tmax
        | otherwise = t

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

    randomNormalizedV3::State StdGen Vf
    randomNormalizedV3 = do
        v <- randomV3 ((-1), 1)
        return $ normalize v

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

    shuffledIndices::Int->State StdGen (Vector Int)
    shuffledIndices n = do
        gen1 <- (state (\gen0 -> split gen0))
        return $ fromList $ shuffle' [0..(n-1)] n gen1

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
        let lCols = drop 3 ls
        let vecCols = fromList $ [(read l)::Float | l <- lCols]
        let (w, h) = (wh!!0, wh!!1)
        cols <- forM [0..(w*h-1)] $ \i -> do
            let r = vecCols!(3*i)
            let g = vecCols!(3*i+1)
            let b = vecCols!(3*i+2)
            return $ V3 (r/maxCol) (g/maxCol) (b/maxCol)
        return $ Image (w, h) cols

    -- for debugging texture
    mkImageFromTexture::(Int, Int)->Texture->Image
    mkImageFromTexture (nx, ny) tex = Image (nx, ny) cols where
        pick = pickColor tex
        xys = [((fromIntegral x)/(fromIntegral nx), (fromIntegral y)/(fromIntegral ny)) | y<-[0..(ny-1)], x<-[0..(nx-1)]]
        cols = [pick (0,0) (V3 x y 0) | (x, y) <- xys]

    gammaCorrection::Image->Image
    gammaCorrection (Image size cols) = Image size $ map (\(V3 r g b)->V3 (r**0.5) (g**0.5) (b**0.5)) cols

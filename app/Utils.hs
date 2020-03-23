module Utils where
    import Control.Monad.State
    import Linear.Vector
    import Linear.V3
    import Linear.Metric
    import Data.Function
    import Data.List
    import System.Random
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

    randomPointInUnitSphere::State StdGen Vf
    randomPointInUnitSphere = do
        x <- randomRng ((-1), 1)
        y <- randomRng ((-1), 1)
        z <- randomRng ((-1), 1)
        let vec = V3 x y z
        if norm vec <= 1 then return vec else randomPointInUnitSphere

    toPPM::Image->String
    toPPM im = unlines(header ++ body)
        where
            Image (nx, ny) cols = im
            header = ["P3",  unwords [show nx, show ny], "255"] -- magic, width, height, maxval
            body = [unwords $ map (\c -> show $ floor $ 255.9*c) [r,g,b] | (V3 r g b) <- cols]

    gammaCorrection::Image->Image
    gammaCorrection (Image size cols) = Image size $ map (\(V3 r g b)->V3 (r**0.5) (g**0.5) (b**0.5)) cols

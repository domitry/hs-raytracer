module Materials where
    import Linear.V3
    import Linear.Vector
    import Linear.Metric
    import Data.Maybe
    import Data.Vector ((!?), (!), fromList, Vector)
    import Data.Bits (xor)
    import System.IO
    import System.Random
    import Control.Monad
    import Control.Monad.State
    import Types
    import Utils

    fromImage::Image->Texture
    fromImage (Image (w, h) cols) = Texture { pickColor=imp_pick }
        where
            vecCols = fromList cols 
            imp_pick (u, v) _ = case (vecCols!?(x+y*w)) of
                Nothing  ->  V3 0.77 0.11 0.54 -- pink if not found
                Just col -> col
                where
                    y = floor $ (1-v)*((fromIntegral h)-0.001)
                    x = floor $ u*(fromIntegral w)

    fromColor::Color->Texture
    fromColor albedo = Texture { pickColor=imp_pick }
        where
            imp_pick uv xy =  albedo

    checker::Color->Color->Texture
    checker col1 col2 = Texture { pickColor=imp_pick }
        where
            imp_pick _ (V3 x y z) = if sign>0 then col1 else col2
                where
                    sign = (sin$10*x)*(sin$10*y)*(sin$10*z)

    -- we need n^3 random vectors, while there are only n vectors
    -- use hash to avoid ocuppying n^3 memory space.
    genLookUp::Int->Vector a->State StdGen ((Int, Int, Int)->a)
    genLookUp n arr = do
        xtab <- shuffledIndices n
        ytab <- shuffledIndices n
        ztab <- shuffledIndices n
        let lookup = \(x, y, z) -> arr!((xtab!x) `xor` (ytab!y) `xor` (ztab!z))
        let limit = \(x, y, z) -> (x `mod` (n-1), y `mod` (n-1), z `mod` (n-1))
        return $ lookup.limit

    -- good article about Perlin noise: http://eastfarthing.com/blog/2015-04-21-noise/
    genPerlin::State StdGen (Vf->Float)
    genPerlin = do
        let n = 256
        vecs <- forM [1..n] (\i->randomNormalizedV3)
        lookup <- genLookUp n (fromList vecs)
        return $ genNoiseFunc n lookup
        where
            genNoiseFunc n lookup = noise where
                noise (V3 x y z) = col where
                    -- hermite cubic function, care that t \in [-1, 1]
                    hcubic t = 1-(3-2*(abs t))*t*t
                    (i, j, k) = (floor x, floor y, floor z)
                    (u, v, w) = (x-fromIntegral i, y-fromIntegral j, z-fromIntegral k)
                    ijks = [(i+di,j+dj,k+dk) | di<-[0,1], dj<-[0,1], dk<-[0,1]]
                    uvws = [(u-di,v-dj,w-dk) | di<-[0,1], dj<-[0,1], dk<-[0,1]]
                    dots = [dot (lookup ijk) (V3 u v w) | (ijk,(u,v,w)) <- zip ijks uvws]
                    hers = [(hcubic u)*(hcubic v)*(hcubic w) | (u, v, w) <- uvws]
                    col = sum [d*h | (d, h) <- zip dots hers] -- [-1, 1]

    accPerlin::(Vf->Float)->Int->(Vf->Float)
    accPerlin noise nlayer xyz = total where
        (total, _, _) = foldl (\(acc, w, p) _ -> 
            (acc+w*(noise p), 0.5*w, 2*^p)) (0, 1, xyz) [1..nlayer]

    perlin::Float->State StdGen Texture
    perlin scale = do
        noise <- genPerlin
        return $ Texture { pickColor=(imp_pick noise) } where
            imp_pick noise _ xyz = ((1+noise (scale*^xyz))/2)*^(V3 1 1 1)

    turbulence::Float->State StdGen Texture
    turbulence scale = do
        noise <- genPerlin
        return $ Texture { pickColor=(imp_pick noise) } where
            imp_pick noise _ xyz = V3 v v v where
                v = abs $ accPerlin noise 7 (scale*^xyz)

    marble::Float->State StdGen Texture
    marble scale = do
        noise <- genPerlin
        return $ Texture { pickColor=(imp_pick noise) } where
            imp_pick noise _ xyz = V3 v v v where
                turb = accPerlin noise 7 xyz
                V3 _ _ z = xyz
                v = (1 + sin (scale*z+10*turb))/2

    -- diffuse
    lambertian::Texture->Material
    lambertian tex = Material { scatter=imp_scatter }
        where
            imp_scatter _ ev = do
                let (n, point, uv) = (evNormal ev, evPoint ev, evUV ev)
                v <- randomPointInUnitSphere
                let v' = n + v
                let col = pickColor tex uv point
                return $ (Just $ Ray point v', col)

    -- be careful that dot vin nv < 0 and nv is normalized
    reflect::Vf->Vf->Vf
    reflect vin n = let vn = abs $ dot vin n in vin + (2*vn)*^n               

    -- be careful that dot vin nv < 0 and nv is normalized
    refract::Vf->Vf->Float->Maybe Vf
    refract nvin nv n_over_n'
        | d > 0 = Just $ (n_over_n')*^hor + (sqrt d)*^vert 
        | otherwise = Nothing
        where
            dt = dot nvin nv
            hor = nvin - dt*^nv -- horizonal comp of vin (sign is the same as vin)
            vert = (-1) *^ nv -- vertical comp of vin (sign is the same as vin)
            d = 1 - (n_over_n' * n_over_n')*(1-dt*dt)

    -- metal
    metal::Color->Float->Material
    metal albedo fuzziness = Material { scatter=imp_scatter }
        where
            imp_scatter (Ray _ vin) ev = do
                let (n, point) = (evNormal ev, evPoint ev)
                let f = bound (0, 1) fuzziness
                s <- randomPointInUnitSphere
                let vout = (reflect vin n) + f*^s
                return $ if (dot vout n) >= 0
                    then (Just $ Ray point vout, albedo)
                    else (Nothing, albedo)
    
    -- glass
    dielectric::Float->Material
    dielectric ri = Material { scatter=imp_scatter }
        where
            schlick cosine ri = r0+(1-r0)*((1-cosine)**5)
                where
                    sqrt_r0 = (1-ri)/(1+ri)
                    r0 = sqrt_r0*sqrt_r0

            imp_scatter (Ray _ vin) ev = do
                let (n, point) = (evNormal ev, evPoint ev)
                let nvin = normalize vin
                let nvinn = dot nvin n
                
                let (normal, n_over_n', cosine) = if nvinn < 0
                    then (n, 1/ri, -nvinn)
                    else ((-1)*^n, ri, nvinn) -- the last element is ri*nvinnn in the book
                
                let reflected = reflect vin normal

                prob <- randomRng (0, 1)
                let prob_reflect = schlick cosine n_over_n'

                let vout = case (refract nvin normal n_over_n') of
                        Nothing -> reflected
                        Just refracted -> if prob < prob_reflect then reflected else refracted
                
                return (Just $ Ray point vout, V3 1 1 1)

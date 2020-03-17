import Control.Monad
import Data.Char
import Data.Function
import Linear

type Color = (Float, Float, Float)g
data Image = Image Int Int [Color]
    deriving Show

genDammyImage::Int->Int->Image
genDammyImage nx ny = Image nx ny [(x/(w-1), 1-y/(h-1), 0.2) | y<-ys, x<-xs]
    where
        w = fromIntegral nx
        h = fromIntegral ny
        xs = map fromIntegral [0..(nx-1)]
        ys = map fromIntegral [0..(ny-1)]

toPPM::Image->String
toPPM im = unlines(header ++ body)
    where
        Image nx ny cols = im
        header = ["P3",  unwords [show nx, show ny], "255"] -- magic, width, height, maxval
        body = [unwords $ map (\c -> show $ floor $ 255.9*c) [r,g,b] | (r,g,b) <- cols]

main = do
    putStr $ toPPM $ genDammyImage 200 100

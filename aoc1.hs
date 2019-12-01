import System.IO  
import Control.Monad


mainf :: FilePath -> IO ()
mainf file = do
    f <- readFile file
    let xs = lines f
    let list = stringtoInt xs
    let res = fuelSum (allFuel list)
    print res   

stringtoInt :: [String] -> [Integer]
stringtoInt = map read

calc :: Integer -> Integer
calc i = (i `div` 3) - 2

allFuel :: [Integer] -> [Integer]
allFuel xs = (map calcExtra xs)

fuelSum :: [Integer] -> Integer
fuelSum [] = 0
fuelSum (x:xs) = x + fuelSum xs

calcExtra :: Integer -> Integer
calcExtra i = let fuel = calc i in 
    case fuel < 0 of
        True -> 0
        otherwise -> fuel + calcExtra fuel


import Data.Char
import Data.Functor
import Data.List

convert :: String -> String
convert s = case groupBy (\a b -> isDigit a == isDigit b) s of
                 [_, r, _, c] -> rc2cr r c
                 [c, r]       -> cr2rc c r
  where rc2cr r c = (chr . (ord 'A' +) <$> reverse (basify (read c) 26)) ++ r
        cr2rc c r = concat ["R", r, "C", show (unbasify (flip (-) (ord 'A') . ord <$> reverse c) 26)]
        basify :: Int -> Int -> [Int]
        basify 0 _ = []
        basify n q = (n - 1) `mod` q : basify ((n - 1) `div` q) q
        unbasify :: [Int] -> Int -> Int
        unbasify [] _     = 0
        unbasify (a:as) q = unbasify as q * q + a + 1

main :: IO ()
main = do
  n <- read <$> getLine
  inp <- loop n
  mapM_ putStrLn $ convert <$> inp
  where loop :: Integer -> IO [String]
        loop 0 = return []
        loop n = do
          input <- getLine
          rest  <- loop $ n - 1
          return $ input : rest

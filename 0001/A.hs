import Control.Arrow
import Data.Functor

answer :: Integer -> Integer -> Integer -> Integer
answer n m a = compute (n, m)
  where compute :: (Integer, Integer) -> Integer
        compute = flip tcount a *** flip tcount a >>> uncurry (*)
        tcount :: Integer -> Integer -> Integer
        tcount a n = (a + n - 1) `div` n

main :: IO ()
main = do
  [n, m, a] <- take 3 . map read . words <$> getLine
  print $ answer n m a

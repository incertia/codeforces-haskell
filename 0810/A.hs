import Data.Functor ((<$>))

main :: IO ()
main = do
  n:k:_ <- readLine
  scores <- readLine
  putStrLn $ show $ solution n k (take (fromInteger n) scores)
  where readLine = map (read :: String -> Integer) <$> (words <$> getLine)

solution :: Integer -> Integer -> [Integer] -> Integer
solution n k scores = max ans 0
  where ans = (2 * k - 1) * n - 2 * s
        s = sum scores

import Control.Monad  (replicateM)
import Data.Functor   ((<$>))
import Data.List      (sortBy)
import Data.Maybe     (fromMaybe)

import qualified Data.ByteString.Char8 as B

newtype ShopDay = ShopDay (Integer, Integer, Integer)
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  n:f:_ <- readLine
  v <- sortBy (flip compare) <$> replicateM (fromInteger n) readDay
  putStrLn $ show $ solution f v
  where readLine = map (fst . fromMaybe undefined . B.readInteger) <$> (B.words <$> B.getLine)
        readDay = do
          a:b:_ <- readLine
          return $ ShopDay (min (2 * a) b - min a b, a, b)

solution :: Integer -> [ShopDay] -> Integer
solution 0 v = sum $ map (\ (ShopDay (_, a, b)) -> min a b) v
solution n (v:vs) = min (2 * a) b + solution (n - 1) vs
  where ShopDay (_, a, b) = v

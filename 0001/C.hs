import Data.Functor

type Pair a = (a, a)

thresh :: RealFloat a => a
thresh = 1e-5

isInt :: RealFloat a => a -> Bool
isInt x = (<=thresh) . abs $ x - (fromIntegral . round) x

dist :: RealFloat a => Pair a -> Pair a -> a
dist (x, y) (x', y') = sqrt $ dx * dx + dy * dy
  where dx = x - x'
        dy = y - y'

distances :: RealFloat a => Pair a -> Pair a -> Pair a -> [a]
distances a b c = [dist a b, dist b c, dist c a]

circumradius :: RealFloat a => Pair a -> Pair a -> Pair a -> a
circumradius a b c = a' * b' * c' / (4 * area)
  where [a', b', c'] = distances a b c
        s = (a' + b' + c') / 2
        area = sqrt $ s * (s - a') * (s - b') * (s - c')

loc :: RealFloat a => a -> a -> a -> a
loc a b c = acos $ (a * a + b * b - c * c) / (2 * a * b)

angles :: RealFloat a => a -> a -> a -> [a]
angles a b c = [loc a b c, loc b c a, loc c a b]

good :: RealFloat a => a -> a -> Bool
good angle angle' = isInt $ angle / angle'

good' :: (RealFloat a, Integral b) => a -> a -> a -> b -> Bool
good' a b c n = and $ flip good angle <$> angles a b c
  where angle = pi / fromIntegral n

good'' :: (RealFloat a, Integral b) => Pair a -> Pair a -> Pair a -> b -> Bool
good'' a b c = good' a' b' c'
  where [a', b', c'] = distances a b c

area :: (RealFloat a, Integral b) => a -> b -> a
area r n = 0.5 * n' * r * r * sin t
  where t = 2 * pi / n'
        n' = fromIntegral n

solution :: RealFloat a => Pair a -> Pair a -> Pair a -> a
solution a b c = head $ area r <$> sides
  where r = circumradius a b c
        sides = map snd $ filter good''' $ (\n -> ((a, b, c), n)) <$> [3..100]
        good''' ((a, b, c), n) = good'' a b c n

main :: IO ()
main = do
  a <- readLine
  b <- readLine
  c <- readLine
  print $ solution a b c
  where readLine = (\[a, b] -> (a, b)) . map read . words <$> getLine

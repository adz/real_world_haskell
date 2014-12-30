import Data.List

boing :: Int -> [Int]
boing n =
  boing $ foldr1 (\x a -> x) $ unfoldr (\x -> if (x < n+10)
                                              then Just (x, x + 1)
                                              else Nothing) n


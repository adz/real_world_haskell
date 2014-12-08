foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f z [] = return z
foldLeftM f z (x:xs)
  = f z x >>= \ z' -> foldLeftM f z' xs


foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f z [] = return z
{-foldRightM f z (x:xs) = f x =<< foldRightM f z xs-}
foldRightM f z (x:xs) = foldRightM f z xs >>= \ xs' -> f x xs'

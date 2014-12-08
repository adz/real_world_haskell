ms = [getChar, getChar]

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
-- good
{-mapM' :: Monad m => (a -> m b) -> [a] -> m [b]-}
{-mapM' f as = sequence (map f as)-}

-- 2: good, just long
{-mapM' :: Monad m => (a -> m b) -> [a] -> m [b]-}
{-mapM' f [] = return []-}
{-mapM' f (a:as) -}
  {-= f a >>= \ b -> mapM' f as >>= \ bs -> return (b : bs)-}

-- throws away results, so m [b] is not result, but m ()
{-mapM' :: Monad m => (a -> m b) -> [a] -> m [b]-}
{-mapM' f as = sequence_ (map f as)-}

{-mapM' :: Monad m => (a -> m b) -> [a] -> m [b]-}
{-mapM' f [] = return []-}
{-mapM' f (a:as)-}
  {-= f a >> \b ->   -- skip rest FAIL-}

-- 5-- fail wrong arrow! -> <-
{-mapM' :: Monad m => (a -> m b) -> [a] -> m [b]-}
{-mapM' f [] = return []-}
{-mapM' f (a:as) =-}
  {-do-}
    {-f a -> b-}
    {-mapM' f as -> bs-}
    {-return (b:bs)-}

-- 6 - good
{-mapM' :: Monad m => (a -> m b) -> [a] -> m [b]-}
{-mapM' f [] = return []-}
{-mapM' f (a:as)-}
  {-= do b <- f a-}
       {-bs <- mapM' f as-}
       {-return (b:bs)-}

-- 7 good
{-mapM' f [] = return []-}
{-mapM' f (a:as)-}
  {-= f a >>=-}
    {-\ b ->-}
      {-do bs <- mapM' f as-}
         {-return (b:bs)-}

-- 8 bad -- reverses application
mapM' f [] = return []
mapM' f (a:as)
  = f a >>=
    \ b ->
      do bs <- mapM' f as
         return (bs ++ [b])

doEeet = do
  mapM' print ['a','b']
  print $ mapM' (\x -> [x]) [1,2,3]

ms = [getChar, getChar]

s1 :: Monad m => [m a] -> m [a]
s1 [] = return []
s1 (m:ms)
  = m >>=
    \ a ->
      do as <- s1 ms
         return (a:as)

-- can't >>= on return () <-- needs to be return []
{-s2 :: Monad m => [m a] -> m [a]-}
{-s2 ms = foldr func (return ()) ms-}
  {-where-}
    {-func :: (Monad m) => m a -> m [a] -> m [a]-}
    {-func m acc-}
      {-= do x  <- m-}
           {-xs <- acc-}
           {-return (x : xs)-}

{-s3 :: Monad m => [m a] -> m [a]-}
{-s3 ms = foldr func (return []) ms-}
  {-where-}
    {-func :: (Monad m) => m a -> m [a] -> m [a]-}
    {-func m acc = m : acc-}

-- invalid syntax
{-s4 :: Monad m => [m a] -> m [a]-}
{-s4 [] = return []-}
{-s4 (m : ms) = return (a : as)-}
  {-where-}
    {-a <- m-}
    {-as <- s4 ms-}

s5 :: Monad m => [m a] -> m [a]
s5 ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc
      = do x <- m
           xs <- acc
           return (x : xs)

-- Can't use '>>' with a lambda on the right!
{-s6 :: Monad m => [m a] -> m [a]-}
{-s6 [] = return []-}
{-s6 (m : ms)-}
  {-= m >>-}
    {-\ a ->-}
      {-do as <- s6 ms-}
         {-return (a : as)-}

-- Can't use <- without do syntax!
{-s7 :: Monad m => [m a] -> m [a]-}
{-s7 (m:ms) = m >>= \a ->-}
  {-as <- s7 ms-}
  {-return (a : as)-}

s8 :: Monad m => [m a] -> m [a]
s8 [] = return []
s8 (m : ms)
  = do a <- m
       as <- s8 ms
       return (a:as)



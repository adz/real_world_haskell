module Lab5 where
import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================
action :: Concurrent a -> Action
action (Concurrent c) = c continuation
    where continuation = (\_ -> Stop)

-- ===================================
-- Ex. 1
-- ===================================
stop :: Concurrent a
stop = Concurrent (\continuation -> Stop)

-- ===================================
-- Ex. 2
-- ===================================
atom :: IO a -> Concurrent a
{-atom io = Concurrent $-}
  {-\continuation ->-}
    {-Atom (io >>= \io_result -> return (continuation io_result))-}

-- Using do notation:
atom io = Concurrent $
  \continuation -> Atom $ do io_result <- io
                             return $ continuation io_result


-- ===================================
-- Ex. 3
-- ===================================
{-forks its argument by -}
{-turning it into an action -}
{-and continues by passing () as the input to the continuation-}
fork :: Concurrent a -> Concurrent ()
-- pretty sure all this is wrong:
{-fork concurrent = Concurrent $ \continuation -> action concurrent-}
{-fork concurrent = Concurrent $ \continuation -> Fork (action concurrent) (action concurrent)-}
{-fork concurrent = \x -> (concurrent (\c -> action concurrent))-}

-- but this *might* be right:
fork concurrent = Concurrent $ \continuation -> Fork (action concurrent) (continuation ())

par :: Concurrent a -> Concurrent a -> Concurrent a
par conc1 conc2 = Concurrent $ \continuation -> Fork (action conc1) (action conc2)

-- ===================================
-- Ex. 4
-- ===================================

-- First work out with unwrapped (no Concurrent)
{-bind :: ((a -> Action) -> Action) -> (a -> ((b -> Action) -> Action)) -> ((b -> Action) -> Action)-}
{-bind f g = \b -> (f (\a -> ((g a) b)))-}
{-bind f g = \b -> (f (\a -> g a b))-}

instance Monad Concurrent where
    -- Type sig is: {-(>>=) :: Concurrent a -> (a -> Concurrent b) -> Concurrent b-}
    -- ...but it's already defined by Monad
    --
    -- First cut:
    (Concurrent f) >>= g = Concurrent $
        \b -> f $ \a -> (get_concurrent_b (g a)) b
        where get_concurrent_b (Concurrent concurrent_b) = concurrent_b

    -- Nicer layout
    (Concurrent f) >>= g =
      Concurrent $
        \b ->
          f $
            \a ->
              (get_concurrent_b (g a)) b
        where
          get_concurrent_b (Concurrent concurrent_b) = concurrent_b

    -- Try using named parts
    (Concurrent f) >>= g = Concurrent $ continuation
        where
          continuation = \b -> f $ \a -> get_bm (g a) b
          get_bm (Concurrent bm) = bm
    return x = Concurrent (\c -> c x)

    -- Try using Alex's case
    (Concurrent f) >>= g = Concurrent $
       \b -> f $ \a -> (case (g a) of (Concurrent bm) -> bm) b
    return x = Concurrent (\c -> c x)

-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin [] = return ()
roundRobin (a:as) = case a of
    Atom io_action -> do next_action <- io_action
                         roundRobin $ as ++ [next_action]
    Fork a1 a2     -> roundRobin $ [a1, a2] ++ as
    Stop           -> roundRobin as

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")

ex2 :: Concurrent ()
ex2 = do atom (putStr "Hello")
         fork (loop $ genRandom 7331) 
         atom (putStrLn " mate")
         loop [1,2,3]


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs


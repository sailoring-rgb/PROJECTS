import Data.List

data LogList a = Log [(String, a)] deriving Show

remLog :: LogList a -> [(String, a)]
remLog (Log x) = x

instance Functor LogList where
  fmap f = let f' = \(s,x) -> (s, f x) in
    Log . (map f') . remLog

instance Applicative LogList where
  pure x = Log [([],x)]
  l1 <*> l2 = Log $ do x <- remLog l1
                       y <- remLog l2
                       g(x,y) where
                         g((s,f),(s',x)) = return (s ++ s', f x)

instance Monad LogList where
  return = pure
  l >>= k = Log $ do x <- remLog l
                     g x where
                       g(s,x) = let u = (remLog (k x)) in map (\(s',x) -> (s ++ s', x)) u

mwrite :: String -> LogList a -> LogList a
mwrite msg l = Log $ let l' = remLog l in map (\(s,x) -> (s ++ msg, x)) l'

{- EXEMPLO:
mwrite (" "++(show x)++" ") (return 1)
-}
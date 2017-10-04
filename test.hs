import Data.List
import Data.Maybe

myLast :: [a] -> Maybe a

myLast [] = Nothing
myLast [x] = Just x
myLast (hd:tl) = myLast tl


{- ggcd :: Integer -> Integer -> Integer -}

ggcd x 0 = x
ggcd x y = ggcd y (mod x y)


table = [("Alice","Bureau 13"),("Bob","Bureau 14")]
bAlice = lookup "Alice" table
bMallory = lookup "Mallory" table

putStrLnM Nothing = putStrLn "Rien"
putStrLnM (Just x) = putStrLn x

putStrLnM2 Nothing = putStrLn "Rien"
putStrLnM2 (Just x) = putStrLn x


putStrLnM3 = \m -> m >>= \x -> Just $ putStrLn x

l = [1..]

{- genL :: [Integer] -> [Integer] -}
genL l = case l of
          [] -> genL [1]
          (hd:tl) -> hd:(genL $ (hd+1):tl)


l2 = let aux n = n:(aux $ n + 1) in aux 1


data MaListe a = Vide | Elem (a,MaListe a)

tete l = case l of
           Vide -> Nothing
           Elem(x,_) -> Just x

tete2 Vide = Nothing
tete2 (Elem(x,_)) = Just x


             


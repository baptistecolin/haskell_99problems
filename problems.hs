import Data.List
import Data.Maybe

myLast :: [a] -> Maybe a

myLast [] = Nothing
myLast [x] = Just x
myLast (hd:tl) = myLast tl
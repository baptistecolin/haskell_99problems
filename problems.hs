import Data.List
import Data.Maybe

myLast :: [a] -> Maybe a

myLast [] = Nothing
myLast [x] = Just x
myLast (hd:tl) = myLast tl

myButLast :: [a] -> Maybe a

myButLast l = case l of
				[] -> Nothing
				[_] -> Nothing
				[a,_] -> Just a
				(hd:tl) -> myButLast tl
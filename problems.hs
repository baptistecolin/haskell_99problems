import Data.List
import Data.Maybe

myLast :: [a] -> Maybe a

myLast [] = Nothing
myLast [x] = Just x
myLast (_:tl) = myLast tl

myButLast :: [a] -> Maybe a

myButLast l = case l of
				[] -> Nothing
				[_] -> Nothing
				[a,_] -> Just a
				(_:tl) -> myButLast tl

elementAt :: [a] -> Integer -> Maybe a

elementAt (hd:_) 1 = Just hd
elementAt [] i = Nothing
elementAt (_:tl) i = elementAt tl (i-1)

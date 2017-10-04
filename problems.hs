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

myLength :: [a] -> Integer

myLength l = case l of
				[] -> 0
				(hd:tl) -> (1+ (myLength tl))

myReverse :: [a] -> [a]

myReverse l = case l of
				[] -> []
				[a] -> [a]
				(hd:tl) -> ((myReverse tl)++[hd])

isPalindrome :: Eq a => [a] -> Bool

isPalindrome l = (l == myReverse l)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]

flatten l = case l of
				Elem a -> [a]
				List [] -> []
				List (hd:tl) -> (flatten hd)++(flatten (List tl))

compress :: Eq a => [a] -> [a]

compress l = case l of
				[] -> []
				[a] -> [a]
				(hd1:(hd2:tl)) -> if hd1==hd2 then compress (hd1:tl) else hd1:(compress (hd2:tl)) 

pack :: Eq a => [a] -> [[a]]

pack [] = []
pack (x:xs) = let (first,rest) = span (==x) xs in (x:first) : pack rest
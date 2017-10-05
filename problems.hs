import Data.List
import Data.Maybe

-- problem 1
myLast :: [a] -> Maybe a

myLast [] = Nothing
myLast [x] = Just x
myLast (_:tl) = myLast tl

-- problem 2
myButLast :: [a] -> Maybe a

myButLast l = case l of
				[] -> Nothing
				[_] -> Nothing
				[a,_] -> Just a
				(_:tl) -> myButLast tl

-- problem 3
elementAt :: [a] -> Integer -> Maybe a

elementAt (hd:_) 1 = Just hd
elementAt [] i = Nothing
elementAt (_:tl) i = elementAt tl (i-1)

-- problem 4
myLength :: [a] -> Integer

myLength l = case l of
				[] -> 0
				(hd:tl) -> (1+ (myLength tl))

-- problem 5
myReverse :: [a] -> [a]

myReverse l = case l of
				[] -> []
				[a] -> [a]
				(hd:tl) -> ((myReverse tl)++[hd])

-- problem 6
isPalindrome :: Eq a => [a] -> Bool

isPalindrome l = (l == myReverse l)

data NestedList a = Elem a | List [NestedList a]

-- problem 7
flatten :: NestedList a -> [a]

flatten l = case l of
				Elem a -> [a]
				List [] -> []
				List (hd:tl) -> (flatten hd)++(flatten (List tl))

-- problem 8
compress :: Eq a => [a] -> [a]

compress l = case l of
				[] -> []
				[a] -> [a]
				(hd1:(hd2:tl)) -> if hd1==hd2 then compress (hd1:tl) else hd1:(compress (hd2:tl)) 

-- problem 9
pack :: Eq a => [a] -> [[a]]

pack [] = []
pack (x:xs) = let (first,rest) = span (==x) xs in (x:first) : pack rest

-- problem 10
encode :: Eq a => [a] -> [(Int, a)]

encode xs = map (\x -> (length x, head x)) (group xs)

data ListItem a = Single a | Multiple Int a
	deriving(Show)

-- problem 11
encodeModified :: Eq a => [a] -> [ListItem a]

encodeModified = map encodeHelper . encode
	where
		encodeHelper (1,a) = Single a
		encodeHelper (n,a) = Multiple n a

-- problem 12
decodeModified :: Eq a => [ListItem a] -> [a]

decodeModified = concat . map decodeAux
	where
		decodeAux (Single x) = [x]
		decodeAux (Multiple 2 x) = x:(decodeAux (Single x))
		decodeAux (Multiple n x) = x:(decodeAux (Multiple (n-1) x))

-- problem 13
-- WIP

-- problem 14
dupli :: [a] -> [a]

dupli [] = []
dupli (hd:tl) = hd:hd:(dupli tl)

--problem 15
repli :: [a] -> Int -> [a]

repli [] _ = []
repli (hd:tl) n = (replicate n hd)++(repli tl n)

--problem 16
myDrop :: [a] -> Int -> [a]

myDrop l n = drop_counter l (n-1) n
	where
		drop_counter [] i n = []
		drop_counter (hd:tl) 0 n = drop_counter tl (n-1) n
		drop_counter (hd:tl) i n = hd:(drop_counter tl (i-1) n)
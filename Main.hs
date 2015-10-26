module Main where

import Data.List
import Data.List.Utils

--------------(  V ,  X ,  Y )
type Action = (Char, Int, Int)
type Board = [Action]

decode :: String -> Board
decode ('l' : msg) = 
    let
    (external, _) = decodeBoard msg []
    in external
decode _ = error "No message"

decodeBoard :: String -> Board -> (Board, String)
decodeBoard ('e':left) acc = (acc, left)
decodeBoard ('d':dict) acc =
    let
    (d,left) = readAction dict
    in decodeBoard left (d:acc)
decodeBoard str ext = error ("Invalid message. Unparsed content: " ++ str) 

readAction :: String -> (Action, String)
readAction str =
    let
        (key1, val1, left) = readItem str
        (key2, val2, left2) = readItem left
        (key3, val3, left3) = readItem left2
        ('e' : left4) = left3
        items = [(key1, val1),(key2, val2),(key3, val3)]
        v1 = match (=='v') items
        v2 = read [(match (=='x') items)] :: Int
        v3 = read [(match (=='y') items)] :: Int
    in ((v1,v2,v3), left4)


readItem :: String -> (Char, Char, String)
readItem str =
    let
        (key, left) = readSymbol str
        (value, left') = readSymbol left
    in (key, value, left')

readSymbol :: String -> (Char, String)
readSymbol ('1' : ':' : letter : rest) = (letter, rest)
readSymbol ('i' : num : 'e' : rest) = (num, rest)
readSymbol str = error ("Invalid message. Unparsed content: " ++ str)

--could use standart lookup function
match :: (Char -> Bool) -> [(Char, Char)] -> Char
match mch [] = error ("Match not found.")
match mch ((b,v) : rest) = 
    if 
        mch b
    then 
        v 
    else (match mch rest)


--sortByX (_, b1, _) (_, b2, _) = compare b1 b2
--sortByY (_, _, c1) (_, _, c2) = compare c1 c2

winner :: String -> Maybe Char
winner [] = Nothing
winner message = if (length winners == 1) then Just (winners !! 0) else Nothing
    where
        mp = decode message

        --xArrays = groupBy (\x y->(getX x) == (getX y)) (sortBy sortByX mp)
        --yArrays = groupBy (\x y->(getY x) == (getY y)) (sortBy sortByY mp)
        xArray1 = mp >>= (\e -> if (getX e) == 0 then [e] else [])
        xArray2 = mp >>= (\e -> if (getX e) == 1 then [e] else [])
        xArray3 = mp >>= (\e -> if (getX e) == 2 then [e] else [])
        yArray1 = mp >>= (\e -> if (getY e) == 0 then [e] else [])
        yArray2 = mp >>= (\e -> if (getY e) == 1 then [e] else [])
        yArray3 = mp >>= (\e -> if (getY e) == 2 then [e] else [])
        xArrays = [xArray1, xArray2, xArray3]
        yArrays = [yArray1, yArray2, yArray3]

        diagon1Array = mp >>= (\e -> if ((getX e) == (getY e)) then [e] else [])
        diagon2Array = mp >>= (\e -> if (getX e == (3-(getY e))) then [e] else [])

        allArrays = merge xArrays $ merge yArrays [diagon1Array, diagon2Array]

        winners = allArrays >>= (\e -> if (length e == 3 && isWinner e) then [(getV (e !! 0))] else [])


isWinner :: Board -> Bool
isWinner mp = if (length grouped == 1) then True else False
    where
        grouped = groupBy (\a b->(getV a) == (getV b)) mp


getV :: Action -> Char
getV (v0,_,_) = v0

getX :: Action -> Int
getX (_,v1,_) = v1

getY :: Action -> Int 
getY (_,_,v2) = v2

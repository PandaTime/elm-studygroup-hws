module Homeworks.Homework1 exposing (
    myLast
  , myButLast
  , elementAt
  , myLength
  , myReverse
  , isPalindrome
  , compress
  , dropEvery
  , clap
  )

{-
  Find last element in a list
  > myLast [1,2,3,4]
  Just 4 
-}
myLast : List a -> Maybe a
myLast list =
  case list of
    [] ->
      Nothing
    [last] ->
      Just last
    head :: rest ->
      myLast rest

{-
  Find prelast element in a list
  > myButLast [1,2,3,4]
  Just 3
-}
myButLast : List a -> Maybe a
myButLast list = 
  case list of
    [] ->
      Nothing
    [_] ->
      Nothing
    [preLast, _] ->
      Just preLast
    head :: rest ->
      myButLast rest
  
{-
  Find the K'th element of a list
  > elementAt [1,2,3] 2
  > Just 2
-}
elementAt : List a -> Int -> Maybe a
elementAt list n =
  if n < 0 then
    Nothing
  else case (n, list) of
    (_, []) ->
      Nothing
    (0, head :: rest) ->
      Just head
    (_, head :: rest) ->
      elementAt rest (n - 1)

{-
  Find length
  > myLength [123, 456, 789]
  Just 3
-}
myLength : List a -> Int
myLength list = 
  case list of
    [] ->
      0
    head :: rest ->
      myLength rest + 1

{-
  Reverse a list
  > myReverse [1,2,3,4]
  [4,3,2,1]
-}

myReverse : List list -> List list
myReverse list =
  case list of
    [] ->
      []
    head :: rest ->
      myReverse rest ++ [head]

{-
  check if palidrom
  > isPalindrome [1,2,4,8,16,8,4,2,1]
  True
-}
isPalindrome : List a -> Bool
isPalindrome list =
  list == myReverse list

{-
  Eliminate consecutive duplicates of string elements
  > compress "aaaabccaadeeee"
  "abcade"
-}
compress : String -> String
compress str =
  let 
    compressArray : List a -> List a
    compressArray list =
      case list of
        [] ->
          []
        [head] ->
          [head]
        head :: second :: rest ->
          if head == second then
            compressArray ([second] ++ rest)
          else
            [head] ++ compressArray ([second] ++ rest)
  in
    String.fromList << compressArray << String.toList <| str

{-
Drop every N'th element from a string
> dropEvery "abcdefghik" 3
"abdeghk"
-}
dropEvery : String -> Int -> String
dropEvery str n =
  let
    dropWhen : List a -> Int -> List a
    dropWhen list curr = 
      case list of
        [] ->
          []
        head :: rest ->
          if curr == n then
            dropWhen rest 1
          else
            [head] ++ dropWhen rest (curr + 1)
  in
    String.fromList << (\list -> dropWhen list 1) << String.toList <| str
    
{-
(optional) Insert the emoji between words
> clap "learning Elm is fun"
"learning 👏 Elm 👏 is 👏 fun"
-}
clap : String -> String
clap str = 
    String.join " " << List.intersperse "👏" << String.words <| str 

{- Results -}
-- main =
--   let
--     task1 = myLast [0,1,2,3,4]
--     task1Result = case task1 of 
--       Just int -> String.fromInt int
--       Nothing -> "Nothing"

--     task2 = myButLast [0,1,2,3,4]
--     task2Result = case task2 of 
--       Just int -> String.fromInt int
--       Nothing -> "Nothing"

--     task3 = elementAt [1,2,3] 2
--     task3Result = case task3 of 
--       Just int -> String.fromInt int
--       Nothing -> "Nothing"

--     task4 = myLength [123, 456, 789]
--     task4Result = String.fromInt task4

--     task5 = myReverse [1,2,3,4]
--     task5Result = String.join ", " (List.map String.fromInt task5)
--   in
--     div [] [ 
--       div [] [ text ("myLast [1,2,3,4] => " ++ task1Result) ],
--       div [] [ text ("myButLast [1,2,3,4] => " ++ task2Result) ],
--       div [] [ text ("elementAt [1,2,3] 2 => " ++ task3Result) ],
--       div [] [ text ("myLength [123, 456, 789] => " ++ task4Result) ],
--       div [] [ text ("myLength [123, 456, 789] => " ++ task5Result) ]
--     ]
--     -- text ("myLast [1,2,3,4] => " ++ hw1String)


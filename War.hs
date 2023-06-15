module War (deal) where

import Data.List

{--
Function stub(s) with type signatures for you to fill in are given below. 
Feel free to add as many additional helper functions as you want. 

The tests for these functions can be found in src/TestSuite.hs. 
You are encouraged to add your own tests in addition to those provided.

Run the tester by executing 'cabal test' from the war directory 
(the one containing war.cabal)
--}
    
deal :: [Int] -> [Int]

deal deck = replace 14 1 (win player1 player2)
-- replace 1 with 14
    where newDeck = replace 1 14 deck
    -- set players and flip deck downwards
          player1 = reverse $ players newDeck 1
          player2 = reverse $ players newDeck 2
-- make a tuple with 2 lists inside
players :: [Int] -> Int -> [Int]
players cards pnum =
    let tuple = (takeEvery 2 cards, dropEvery 2 cards)
    in if pnum == 1 -- 1 is first player and 2 is second player
       then  fst tuple
       else  snd tuple

--helper functions to seperate players
takeEvery :: Int -> [a] -> [a]
takeEvery n = map snd . filter ((== 1) . fst) . zip (cycle [1..n])
       
dropEvery :: Int -> [a] -> [a]
dropEvery n = map snd . filter ((/= 1) . fst) . zip (cycle [1..n])

-- replace function
replace :: Int -> Int -> [Int] -> [Int]
replace from to cards = [if num == from then to else num | num <- cards]
-- the other player wins when one is empty
win :: [Int] -> [Int] -> [Int]
win p1wins [] = p1wins
win [] p2wins = p2wins
  
win p1 p2
-- function to put cards at the bottom of the play
  | head p1 < head p2 = win (tail p1) ((tail p2) ++ [head p2, head p1])
  | head p1 > head p2 = win ((tail p1) ++ [head p1, head p2]) (tail p2)
  -- go to war when equal
  | otherwise = war p1 p2 []

-- when ending in war, returns winner
war [] [] tie = tie
war p1 [] list = win (p1 ++ list) []
war [] p2 list = win [] (p2 ++ list)

war p1 p2 list
-- war at the end when a player has 1 card
  | (length p1 == 1 || length p2 == 1) && (head p1 == head p2) =
      let list' = list ++ [head p1, head p2]
          p1'= drop 1 p1
          p2'= drop 1 p2
      in war p1' p2' (reverse $ sort list')
      -- war wn both players can face face up and down the cards 
  | (length p1 >= 2 && length p2 >= 2) && (head p1 == head p2) =
      let list' = list ++ [head p1, head p2, head(tail p1), head(tail p2)]
          p1'= drop 2 p1
          p2'= drop 2 p2
      in war p1' p2' (reverse $ sort list')
-- after war is over, comparing the upright cards
  | otherwise =
      let list' = list ++ [head p1, head p2]
      in if head p1 < head p2 
         then win (tail p1) ((tail p2) ++ (reverse $ sort list'))
         else win ((tail p1) ++ (reverse $ sort list')) (tail p2) 




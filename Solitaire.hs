module Solitaire where

--Import libraries
import Data.List
import Data.List.Split
import System.Random



--declare data types 
data Suit = Hearts|Diamonds|Clubs|Spades
  deriving (Eq,Show,Enum) 
data Pip = Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
  deriving(Eq,Show,Enum,Ord) 
type Card = (Pip,Suit)
type Deck = [Card]
type Foundation = [[Card]]
type Column = [[Card]]
type Reserve = [Card]
type EOBoard = (Foundation,Column,Reserve)


-- generates a pack which is in order
pack :: Deck
pack = [(p,s)|p<-[(Ace)..(King)], s<-[(Hearts)..(Spades)]]

--takes a card and returns the successor
sCard :: Card -> Card
sCard(p,s) = (succ p, s)

--takes a card and returns the predecesor
pCard :: Card -> Card
pCard(p,s) = (pred p, s)

--takes a card and returns true if it's an ace
isAce :: Card -> Bool
isAce(p,s)
  |p == Ace = True
  |otherwise = False

--takes a card and returns true if it's a king
isKing :: Card -> Bool
isKing(p,s)
  |p == King = True
  |otherwise = False 




--takes an int as a seed and shuffles the deck 
shuffle :: Int -> Deck
shuffle seed = map fst sl
  where ri = take 52 (randoms (mkStdGen seed)::[Int])
        zl = zip pack ri
        sl = sortBy (\(_,n1) (_,n2) -> compare n1 n2) zl


--builds an EOBoard 
eODeal :: Int -> EOBoard
eODeal seed = let
              sp = shuffle seed
              c=chunksOf 6 (drop 4 (shuffle seed))
              f = []
              r = take 4 (shuffle seed)
              in (f,c,r)

--checks if you can make a move 
--if you can move recusively call the function on the updated board
--until no moves are available
toFoundations :: EOBoard -> EOBoard
toFoundations (f,c,r)
  |canMove (f,c,r) = toFoundations uBoard --recurse on updated board
  |otherwise = (f,c,r)
  where uBoard = foldr (move) (f,c,r) p --tries to move all movable cards
        p = [head p | p<- filter(\co -> co/=[]) c] ++ r --returns all movable cards



--checks if an EOBoard has available moves
canMove :: EOBoard -> Bool
canMove (f,c,r)
  |filter (\card -> canMoveA f card) p /=[] = True 
  |otherwise = False
  where p = [head p | p<- filter(\co -> co/=[]) c] ++ r --returns all movable cards

--auxiliary function for canMove
--takes in a foundation and a card and returns true if the card can be move to foundation
--checks if a card is either an Ace or the successor of one of the top cards from foundations
canMoveA :: Foundation -> Card -> Bool
canMoveA f c  
  |isAce c = True --is Ace?
  |elem (pCard c) hf = True --is successor?
  |otherwise = False
  where hf = [head l | l<- f ]--returns the heads of the foundations lists

--takes a card and a board and moves 
--the card to foundations if it's an ace
--otherwise call auxiliary function 
move :: Card -> EOBoard ->EOBoard
move card (f,c,r)
   |isAce card = ([card]:f,delc,delete card r)--update board 
   |otherwise = moveA card (f,c,r)
   where delc = [delete card l|l<-c]--deletes a card from columns

--auxiliary function for move
--takes a card and a board and finds where in foundations it belongs
--and moves it there while deleting the card from columns/reserve 
moveA :: Card -> EOBoard -> EOBoard
moveA card (f,c,r)
   |f==[]= (f,c,r)
   |head (head f) == pCard card = (([card]: f),delc, delete card r)
   |otherwise = (h: f1, c1 , r1 )
   where delc = [delete card l | l<-c]--deletes a card from the columns
         (h:t) = f
         (f1,c1,r1) = moveA card (t,c,r)--recurses on the tail of foundations  

          












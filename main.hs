import Data.List
import Data.Maybe
import Test.QuickCheck

data Suit = Hearts | Diamonds | Spades | Clubs deriving (Show, Eq, Ord)  
data Rank = A | K | Q | J | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two deriving (Show, Eq, Ord)  
data Card = Card {rank :: Rank, suit :: Suit} deriving (Show, Eq, Ord)  
type Kicker = Rank
data Hand = TwoPairs (Rank, Rank) | Pair Rank | HighestCard Rank deriving (Show, Eq, Ord)  

groupDuplicates :: Eq a => [a] -> [[a]]
groupDuplicates [] = []
groupDuplicates [x] = [[x]]
groupDuplicates (x:xs) = (x : [z | z <- xs, z == x]) : (groupDuplicates [z | z <- xs, z /= x])

single :: [a] -> Maybe a
single [x] = Just x
single _ = Nothing

double :: [a] -> Maybe (a, a)
double [x, y] = Just (x, y)
double _ = Nothing

rankGroupsOf :: Int -> [Card] -> [Rank]
rankGroupsOf quantity cards = (map head pairs)
	where
		pairs = [x | x <- (groupDuplicates ranks), (length x) == quantity]
		ranks = map rank cards

highestCard :: [Card] -> Maybe Hand 
highestCard = Just . HighestCard . rank . head . sort

pair :: [Card] -> Maybe Hand
pair cards = (single . (rankGroupsOf 2) $ cards) >>= (Just . Pair)

twoPairs :: [Card] -> Maybe Hand
twoPairs cards = (double . (rankGroupsOf 2) $ cards) >>= (Just . TwoPairs)

identifyHand :: [Card] -> Hand
identifyHand cards = head . sort $ hands
	where hands = catMaybes $ map ($ cards) [highestCard, pair, twoPairs]

main = do 
	let cards = [(Card A Hearts), (Card Q Diamonds), (Card K Diamonds), (Card J Spades)]
	print $ identifyHand cards

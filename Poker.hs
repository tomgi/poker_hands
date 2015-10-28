module Poker where

	import Data.List
	import Data.Maybe
	import Control.Applicative
	import Test.QuickCheck

	data Suit = Hearts | Diamonds | Spades | Clubs deriving (Show, Eq, Ord)  
	data Rank = A | K | Q | J | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two deriving (Show, Eq, Ord, Enum)  
	data Card = Card {rank :: Rank, suit :: Suit} deriving (Show, Eq, Ord)  
	type Kicker = Rank
	data Hand = StraightFlush [Rank] Suit | FourOfAKind {ofRank :: (Rank), kicker :: Kicker} | FullHouse Rank Rank | Flush (Suit) | Straight [Rank] | ThreeOfAKind {ofRank :: (Rank), kicker :: Kicker} | TwoPairs {ofRanks :: (Rank, Rank), kicker:: Kicker } | Pair {ofRank :: Rank,  kicker :: Kicker} | HighestCard Rank deriving (Show, Eq, Ord)  

	rankOrder :: [Rank]
	rankOrder = [A .. Two] ++ [A]

	remove :: Eq a => a -> [a] -> [a]
	remove = filter . (/=)

	removeAll :: Eq a => [a] -> [a] -> [a]
	removeAll exclude xs = [z | z <- xs, not(z `elem` exclude)]

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

	highestRank :: [Card] -> Rank
	highestRank = rank . head . sort

	highestCard :: [Card] -> Hand 
	highestCard = HighestCard . highestRank

	getKickerExceptRank :: Rank -> [Card] -> Kicker
	getKickerExceptRank p cards = head . sort $ remove p (map rank cards)

	pair :: [Card] -> Maybe Hand
	pair cards = (single . (rankGroupsOf 2) $ cards) >>= (\p -> Just $ Pair p $ getKickerExceptRank p cards)

	threeOfAKind :: [Card] -> Maybe Hand
	threeOfAKind cards = (single . (rankGroupsOf 3) $ cards) >>= (\t -> Just $ ThreeOfAKind t $ getKickerExceptRank t cards)

	fourOfAKind :: [Card] -> Maybe Hand
	fourOfAKind cards = (single . (rankGroupsOf 4) $ cards) >>= (\f -> Just $ FourOfAKind f $ getKickerExceptRank f cards)

	twoPairs :: [Card] -> Maybe Hand
	twoPairs cards = (double . (rankGroupsOf 2) $ cards) >>= (\ps -> (Just $ TwoPairs ps $ head . sort $ removeAll [fst ps, snd ps] (map rank cards)))

	fullHouse :: [Card] -> Maybe Hand
	fullHouse cards = do
		(Pair pair _) <- pair cards
		(ThreeOfAKind three _) <- threeOfAKind cards
		return $ FullHouse three pair

	flush :: [Card] -> Maybe Hand
	flush cards = (single . nub . map suit $ cards) >>= (Just . Flush)

	straight :: [Card] -> Maybe Hand
	straight cards
		| isInfixOf ranks rankOrder 	= (Just . Straight $ ranks)
		| otherwise						= Nothing
		where ranks = (sort $ map rank cards)

	straightFlush :: [Card] -> Maybe Hand
	straightFlush cards = do
		(Flush suit) <- flush cards
		(Straight ranks) <- straight cards
		return $ StraightFlush ranks suit

	identifyHand :: [Card] -> Hand
	identifyHand cards = fromMaybe defaultHand possibleHands		
		where 
			defaultHand = highestCard cards
			possibleHands = 
				(straightFlush cards) <|>
				(fourOfAKind cards) <|>
				(fullHouse cards) <|>
				(flush cards) <|>
				(straight cards) <|>
				(threeOfAKind cards) <|> 
				(twoPairs cards) <|> 
				(pair cards)
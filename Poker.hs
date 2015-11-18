module Poker (
		Suit(..),
		Rank(..),
		Card(..),
		Kicker(..),
		HandCards(..),
		Hand(..),
		identifyHand
	)where

	import Data.List
	import Data.Maybe
	import Control.Applicative
	import Test.QuickCheck

	data Suit = Hearts | Diamonds | Spades | Clubs deriving (Show, Eq, Ord) 
	data Rank = A | K | Q | J | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two deriving (Show, Eq, Ord, Enum) 
	data Card = Card {rank :: Rank, suit :: Suit} deriving (Show, Eq, Ord) 
	type Kicker = Rank
	data HandCards = HandCards Card Card Card Card Card
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

	toArray :: HandCards -> [Card]
	toArray (HandCards a b c d e) = [a, b, c, d, e]

	highestRank :: HandCards -> Rank
	highestRank = rank . head . sort . toArray

	highestCard :: HandCards -> Hand 
	highestCard = HighestCard . highestRank

	getKickerExceptRank :: Rank -> HandCards -> Kicker
	getKickerExceptRank p cards = head . sort $ remove p (map rank (toArray cards))

	pair :: HandCards -> Maybe Hand
	pair cards = (single . (rankGroupsOf 2) $ toArray cards) >>= (\p -> Just $ Pair p $ getKickerExceptRank p cards)

	threeOfAKind :: HandCards -> Maybe Hand
	threeOfAKind cards = (single . (rankGroupsOf 3) $ toArray cards) >>= (\t -> Just $ ThreeOfAKind t $ getKickerExceptRank t cards)

	fourOfAKind :: HandCards -> Maybe Hand
	fourOfAKind cards = (single . (rankGroupsOf 4) $ toArray cards) >>= (\f -> Just $ FourOfAKind f $ getKickerExceptRank f cards)

	twoPairs :: HandCards -> Maybe Hand
	twoPairs cards = (double . (rankGroupsOf 2) $ toArray cards) >>= (\ps -> (Just $ TwoPairs ps $ head . sort $ removeAll [fst ps, snd ps] (map rank (toArray cards))))

	fullHouse :: HandCards -> Maybe Hand
	fullHouse cards = do
		(Pair pair _) <- pair cards
		(ThreeOfAKind three _) <- threeOfAKind cards
		return $ FullHouse three pair

	flush :: HandCards -> Maybe Hand
	flush cards = (single . nub . map suit . toArray $ cards) >>= (Just . Flush)

	straight :: HandCards -> Maybe Hand
	straight cards
		| isInfixOf ranks rankOrder 	= (Just . Straight $ ranks)
		| otherwise						= Nothing
		where ranks = (sort $ map rank (toArray cards))

	straightFlush :: HandCards -> Maybe Hand
	straightFlush cards = do
		(Flush suit) <- flush cards
		(Straight ranks) <- straight cards
		return $ StraightFlush ranks suit

	identifyHand :: HandCards -> Hand
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
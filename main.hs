module Main (main) where

	import Poker

	main = do 
		let cards = HandCards (Card A Clubs) (Card Q Clubs) (Card J Clubs) (Card Ten Clubs) (Card K Clubs)
		print $ identifyHand cards

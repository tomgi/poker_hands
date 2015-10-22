module Main (main) where

	import Poker

	main = do 
		let cards = [(Card A Hearts), (Card A Clubs), (Card A Spades), (Card J Hearts), (Card J Spades)]
		print $ identifyHand cards

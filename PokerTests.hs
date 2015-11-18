module PokerTests where

	import Poker
	import Test.Hspec
	import Test.QuickCheck

	main :: IO ()
	main = hspec $ do
	  describe "Poker.identifyHand" $ do
	    it "works for pair" $ do
	      (identifyHand $ HandCards (Card A Hearts) (Card A Clubs) (Card Eight Spades) (Card J Hearts) (Card Ten Spades)) `shouldBe` (Pair A J)

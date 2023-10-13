{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Main where
import Test.Hspec ( hspec, describe, it, shouldBe, Spec )
import Data.Option


main :: IO ()
main = hspec $ do
  describe "Functor" $ do
    it "fmap (+1) (Some 1) == Some 2" $
      fmap (+1) (Some 1) `shouldBe` Some (2 :: Int)
    it "fmap (+1) None == None" $
      fmap not None `shouldBe` (None :: Option Bool)
  
  describe "Applicative" $ do
    it "pure (+) <*> (Some 1) <*> (Some 1) == Some 2" $
      pure (+) <*> (Some 1) <*> (Some 1) `shouldBe` (Some 2 :: Option Int)
    it "pure (+) <*> None <*> (Some 1) == None" $
      pure (+) <*> None <*> (Some 1) `shouldBe` (None :: Option Int)
    it "pure (+) (Some 1) None == None" $
      pure (+) <*> (Some 1) <*> None `shouldBe` (None :: Option Int)

  describe "Monad" $ do
    it "Some 1 >>= (\\i -> Some (i + 1)) == Some 2" $
      (Some 1 >>= (\i -> Some (i + 1))) `shouldBe` (Some 2 :: Option Int)
    it "None >>= (\\i -> Some (i + 1)) == None" $
      (None >>= (\i -> Some (i + 1))) `shouldBe` (None :: Option Int)
    it "Some True >>= (\\_ -> None) == None" $
      (Some True >>= (\_ -> None)) `shouldBe` (None :: Option Int)
  
  describe "Monadic banking (>>=)" $ do
    it "balanceByEmail' \"daniel.kroeni@fhnw.ch\" == balanceByEmail'' \"daniel.kroeni@fhnw.ch\"" $
      balanceByEmail' "daniel.kroeni@fhnw.ch" `shouldBe` balanceByEmail'' "daniel.kroeni@fhnw.ch"
    it "balanceByEmail' \"hans@noaccou.nt\" == balanceByEmail'' \"hans@noaccou.nt\"" $
      balanceByEmail' "hans@noaccou.nt" `shouldBe` balanceByEmail'' "hans@noaccou.nt"
    it "balanceByEmail' \"peter@unkno.wn\" == balanceByEmail'' \"peter@unkno.wn\"" $
      balanceByEmail' "peter@unkno.wn" `shouldBe` balanceByEmail'' "peter@unkno.wn"
  
  describe "Monadic banking (do-notation)" $ do
    it "balanceByEmail' \"daniel.kroeni@fhnw.ch\" == balanceByEmail \"daniel.kroeni@fhnw.ch\"" $
      balanceByEmail' "daniel.kroeni@fhnw.ch" `shouldBe` balanceByEmail "daniel.kroeni@fhnw.ch"
    it "balanceByEmail' \"hans@noaccou.nt\" == balanceByEmail \"hans@noaccou.nt\"" $
      balanceByEmail' "hans@noaccou.nt" `shouldBe` balanceByEmail "hans@noaccou.nt"
    it "balanceByEmail' \"peter@unkno.wn\" == balanceByEmail \"peter@unkno.wn\"" $
      balanceByEmail' "peter@unkno.wn" `shouldBe` balanceByEmail "peter@unkno.wn"

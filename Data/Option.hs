{-# LANGUAGE InstanceSigs #-}
module Data.Option where
import Prelude hiding (lookup)

todo :: a
todo = error "TODO"

-- An `Option a` models a value of type `a` which may be missing.
data Option a = None | Some a deriving (Show, Eq)

-- Now implement Functor, Applicative and Monad
instance Functor Option where
    fmap :: (a -> b) -> Option a -> Option b
    fmap = todo

instance Applicative Option where
    pure :: a -> Option a
    pure = todo

    (<*>) :: Option (a -> b) -> Option a -> Option b
    (<*>) = todo

instance Monad Option where
  (>>=) :: Option a -> (a -> Option b) -> Option b
  (>>=) = todo

-- TODO: Check your definitions by running the tests:
-- > cabal test option --test-show-details=direct

-------------------------------------------------------------------------------
-- Example: Bank accounts
-------------------------------------------------------------------------------

data Account = MkAccount { userId :: Int, balance :: Double }

users :: [(String, Int)]
users = [
  ("daniel.kroeni@fhnw.ch", 42),
  ("hans@noaccou.nt", 43)]

accounts :: [Account]
accounts = [
 MkAccount 1 5,
 MkAccount 41 20,
 MkAccount 42 0]


lookup :: Eq a => a -> [(a,b)] -> Option b
lookup _ [] = None
lookup key ((k,v):kvs) 
  | key == k = Some v
  | otherwise = lookup key kvs


userIdByEmail :: String -> Option Int
userIdByEmail email = lookup email users 


accountByUserId :: Int -> Option Account
accountByUserId uid = lookup uid accountKVs 
  where accountKVs = map (\a -> (userId a, a)) accounts


balanceByEmail' :: String -> Option Double
balanceByEmail' email =
  case userIdByEmail email of
    None -> None
    Some uid -> case accountByUserId uid of
      None -> None
      Some acc -> Some (balance acc)

-- TODO: Implement the same functionality as above using `>>=` 
-- instead of pattern matching:
balanceByEmail'' :: String -> Option Double
balanceByEmail'' email = todo

-- TODO: Implement the same functionality as above using do-notation:
balanceByEmail :: String -> Option Double
balanceByEmail email = do
  todo

main :: IO ()
main = do
  print $ balanceByEmail "daniel.kroeni@fhnw.ch"


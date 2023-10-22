import Prelude hiding (Maybe(..), lookup)

-- In this exercise you will refactor some code which explicitly 
-- passes configuration data around into a program threads the
-- configuration data through implicitly using a Reader Monad.

-- The configuration data for our application contains a Bool stating 
-- whether or not we're in development mode.                  
newtype Config = Config { devStage :: Bool }

-- This function takes a name and a config and depending on the config
-- it returns a dummy email address or a real one.
createAddr' :: String -> Config -> String
createAddr' name config =
  name ++ if devStage config then "@dummy.com" else "@fp.io"

-- This function takes an email address and a config and depending on
-- the config it returns a dummy email body or a real one.
createBody' :: String -> Config -> String
createBody' addr config =
   "mailto:" ++ addr ++ "\n" ++ if devStage config then "Test" else "Dear .."

-- This function takes a name and a config and depending on the config
-- it returns a dummy email or a real one.
createMail' :: String -> Config -> String
createMail' name config = createBody' (createAddr' name config) config


-- Expected value: "mailto:daniel@fp.io\nDear .."
test :: String
test = createMail' "daniel" (Config False)
 

-- Observe that the Config parameter is passed to almost every function
-- as the last parameter (before the return type).
-- Our goal is to get rid of the Config parameter in all functions by
-- refactoring the code such that the functions return a value of type
-- `Reader Config a` instead of a value of type a.

newtype Reader env a = Reader { runReader :: env -> a }

ask :: Reader env env
ask = Reader (\env -> env)

-- Here is the Functor instance:
instance Functor (Reader env) where
  fmap :: (a -> b) -> Reader env a -> Reader env b
  fmap f (Reader g) = Reader (\env -> f (g env)) -- f.g -- runReader could be used

-- This is the Applicative instance:
instance Applicative (Reader env) where
  pure :: a -> Reader env a
  pure a = Reader (\env -> a)

  (<*>) :: Reader env (a -> b) -> Reader env a -> Reader env b
  (Reader f) <*> (Reader a) = Reader (\env -> f env (a env))

-- This is the Monad instance:
instance Monad (Reader env) where
  (>>=) :: Reader env a -> (a -> Reader env b) -> Reader env b
  (Reader a) >>= f = Reader (\env -> runReader (f (a env)) env)   


createAddr :: String -> Reader Config String
createAddr name = do
  config <- ask 
  return $ name ++ if devStage config then "@dummy.com" else "@fp.io"


createBody :: String -> Reader Config String
createBody addr = do
  config <- ask
  return $ "mailto:" ++ addr ++ if devStage config 
                      then "Test" else "Dear .."

createMail :: String -> Reader Config String
createMail name = do
  addr <- createAddr name
  createBody addr

-- createAddr name >>= \addr -> createBody addr

testReader = runReader readerAction (Config False)
  where readerAction = createMail "daniel"


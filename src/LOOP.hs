module LOOP where

import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.IntMap         as Map
import qualified Data.List           as List
import           Data.Maybe          (fromMaybe)
import           Data.Monoid

type Var = Int

type Nat = Integer
type Vars = Map.IntMap Nat

infixl 8 :-

data LOOP
  = INC Var Nat
  | DEC Var Nat
  | LOOP :- LOOP
  | LOOP Var LOOP
    deriving Show

cat :: [LOOP] -> LOOP
cat = List.foldr1 (:-)


-- LOOP interpreter

type LM a = StateT Vars IO a

lookupVar :: Var -> LM Nat
lookupVar var = do
  vars <- get
  return $ fromMaybe 0 (Map.lookup var vars)

chVal :: Var -> (Nat -> Nat) -> StateT Vars IO ()
chVal var f = do
  vars <- get
  v <- lookupVar var
  let new = max 0 $ f v
  -- liftIO $ putStrLn $ show var ++ " := " ++ show new
  put $ Map.insert var new vars

runLOOP :: Vars -> LOOP -> IO Vars
runLOOP start m = fmap snd . flip runStateT start $ go m
  where
    go :: LOOP -> LM ()
    go m' = case m' of
      INC var n -> chVal var (\x -> x + n)
      DEC var n -> chVal var (subtract n)
      m :- n -> go m >> go n
      LOOP x m -> do
        n <- lookupVar x
        replicateM_ (fromIntegral n) (go m)

-- LOOP combinators

(+=) :: Var -> Var -> LOOP
a += b = LOOP b (INC a 1)


(-=) :: Var -> Var -> LOOP
a -= b = LOOP b (DEC a 1)

zero :: Var -> LOOP
zero a = a -= a

(=:) :: Var -> Var -> LOOP
a =: b =  zero a
       :- a += b

sgn :: Var -> Var -> LOOP
sgn helper a =
     helper =: a
  :- DEC helper 1
  :- a -= helper

ifnzero :: Var -> Var -> LOOP -> LOOP
ifnzero helper a m = sgn helper a :- LOOP a m

ifgt :: Var -> Var -> Var -> LOOP -> LOOP
ifgt helper a b m =   a -= b
                  :- ifnzero helper a m

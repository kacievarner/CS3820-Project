module Rules where

import Control.Monad.State
import Data.List (sort)
import System.Environment (getArgs)


data Term =
    Var String
    | Atom String
    | Rule String [Term]
    | Query String [Term] deriving Show




a = Var "a"
b = Var "b"

edge1 = Rule "edge" [a,b]
query1 = Query "edge" [a,b]


program :: Statements
program = [
    edge1,
    query1
    ]

type Statements = [Term]

data Relation = Rel String [Term] 

type Env x = StateT [Relation] IO x

names :: [Term] -> [String]
names ((Var x) : xs) = x : names xs
names _ = []

ruleHolds :: String -> [Relation] -> [String] -> Bool
ruleHolds name [] vars = False
ruleHolds name ((Rel x related) : xs) vars
    | x == name = relHolds (names related) vars
    | otherwise = ruleHolds name xs vars

relHolds :: [String] -> [String] -> Bool
relHolds xs ys = sort xs == sort ys

eval :: Term -> Env ()
eval (Var s) = return ()
eval (Atom s) = return ()
eval (Rule name ts) = do
    env <- get
    put $ (Rel name ts) : env
eval (Query name ts) = do 
    env <- get
    let holds = ruleHolds name env (names ts)
    if holds
        then liftIO $ putStrLn "Yes!"
        else liftIO $ putStrLn "No!"
    return ()


evalT :: Statements -> Env ()
evalT stmts = (mapM_ eval) []

-- eval' :: Statements -> [Relation]
eval' = execStateT . eval 



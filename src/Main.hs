{-# OPTIONS_GHC -Wno-missing-fields #-}
module Main where

import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Bird.Parser
import Bird.Printer
import Data.Char
import Data.List (intercalate)

{------------------------------------------------------------------------}
--Main function
main :: IO ()
main = do args <- getArgs
          let files = filter ("-p" /=) args
              go | elem "-p" args = goParse
                 | otherwise      = goEval
          when (not (null files)) $
              go  . concat =<< mapM readFile files

{------------------------------------------------------------------------}
--Evaluation functions
goEval :: String -> IO ()
--Evaluates the functions similar to how goParse evaluates
goEval s = putStrLn (evalFunction [] ((fst . head)(runParser(many mainParser)s))) --Evaluates until empty

--Function recursively evaluates a given list until there is nothing left
evalFunction :: [Complex] -> [Complex] -> [Char]
evalFunction parseList [] = "" --Base case for when the function has evaluated all the terms and receives an empty list
evalFunction parseList xs = snd final ++ evalFunction(fst final)(tail xs) --Evaluates from head to tail as defined below until empty
                where final = evalFunctionHelper parseList (head xs) --Recursively evaluates the term at the head of the list until it is empty

--Helper identifies the type of complex that is used in the list and evaluates it to a yes or no
evalFunctionHelper :: [Complex] -> Complex -> ([Complex], String)
evalFunctionHelper complexTerms (Predicate xs) = (complexTerms ++ [Predicate xs], "\n") --Pattern matching on predicates
evalFunctionHelper complexTerms (Rule x xs) = (complexTerms ++ [Rule x xs], "\n") --Pattern matching on rules
evalFunctionHelper complexTerms (Query x) = (complexTerms, final) --Pattern matching on queries/starts the eval "loop"
                        where final = if Predicate x `elem` complexTerms --Evaluation/comparing of the terms to terms defined in helper
                                then "Yes." --Print yes
                                else "No." --Print no
evalFunctionHelper complexTerms (Comment x) = (complexTerms, "\n") --Needed because of complex data type, but ignoring for now

{------------------------------------------------------------------------}
--Main parsers
goParse :: String -> IO ()
--Parses only complex functions as defined below, AST functions are called inside complex function
goParse s = mapM_ (putStrLn . printComplex)((fst . head)(runParser(many mainParser)s)) --Need to not evaluate line by line

mainParser :: Parser Complex --Complex terms and comment removal
mainParser = query <|> predicate <|> rule <|> removeComment

subParser :: Parser AST --All basic AST terms and functions
subParser = var <|> atom <|> intParser <|> listParser <|> str <|> barListParser

{------------------------------------------------------------------------}
--Printer statements for parsing
printAst :: AST -> String
printAst (Atom x) = x --Returns itself
printAst (Variable x) = x --Returns itself
printAst (Str x) = '\"' : x ++ ['\"'] --String returns list of strings
printAst (List x) = intercalate "" (map printAst x) --List denoted by empty string
printAst (Compound x xs) =  printAst x ++ "(" ++ intercalate "," ( map printAst xs) ++ ")" --Compound denoted by parens

printComplex :: Complex -> String
printComplex (Query xs) = intercalate "," (map printAst xs) ++ "?" --Queries end in ?
printComplex (Rule x xs) = printAst x ++ ":-" ++ intercalate "," (map printAst xs) ++ "." --Rule is separated by :- and ends in .
printComplex (Predicate x) = intercalate "," (map printAst x) ++ "." --Predicate ends in .
printComplex (Comment x) = "%" ++ unwords (map printAst x) --Comments denoted by beginning with %

{------------------------------------------------------------------------}
--AST definitions for terms and their types
--Simple inner function terms
data AST = Atom String
        | Variable String
        | Str String
        | Compound AST [AST]
        | List [AST] deriving (Show, Eq) --Includes bar list and regular list parsing

--Complex outer/inner function terms
data Complex = Predicate [AST]
        | Rule AST [AST]
        | Query [AST]
        | Comment [AST] deriving (Show, Eq) --Attached to sole function removeComment

{------------------------------------------------------------------------}
--Functions for evaluating terms: Atom, Variable, Str, and Compound
atom :: Parser AST
atom =  do
        x <- token lower --Begins with lowercase letter
        xs <- many(lower <|> upper) --Subsequent characters are case insensitive
        return(Atom(x:xs)) --Parse atom/string from beginning to end

-- >>> runParser atom "a"
-- [(Atom "a","")]

var :: Parser AST
var =  do
        x <- token upper --Begins with an uppercase letter
        xs <- many lower --Subsequent characters are lowercase
        return(Variable(x:xs)) --Parse variable/string from beginning to end

-- >>> runParser var "Test"
-- [(Variable "Test","")]

str :: Parser AST
str = do
        x <- token(satisfy(== '\"' )) --Beginning of string starts with a quote
        y <- many1(satisfy (/= '\"')) --Middle of string can be anything but a quote
        z <- satisfy(== '\"') --End of string starts with a quote
        return(Str y) --Returns y, which is anything defined between the quotes

-- >>> runParser str "\"hello\""
-- [(Str "hello","")]

compound :: Parser AST
compound = do
        x <- token subParser --Begins with a term as defined in subParser
        xs <- token(parens(sepBy1(token(string ","))(token compound))) --Separates defined terms by "," recurses on compound to take another term
        return(Compound x xs) --Returns compound composed of a term composed of a term
        <|> do token subParser

--Encapsulation process, evaluates atom succ, then recurses on atom term succ again, and ends at atom zero
-- >>> runParser compound "succ(succ(zero))"
-- [(Compound (Atom "succ") [Compound (Atom "succ") [Atom "zero"]],"")]
--

--Doesn't work because it takes more than 1 complex function to process this evaluation
-- >>> runParser compound "p(a)? p(b)? p(c)? p(d)? p(e)? p(f)?"
-- [(Compound (Atom "p") [Atom "a"],"? p(b)? p(c)? p(d)? p(e)? p(f)?")]

{--------------------------------------------------------------------------}
--Predicate Parser
predicate :: Parser Complex
predicate = do
        x <- sepBy1(string ",")compound --Evaluates terms separated by "," calls compound for stuff in ()
        punctuation <- token (satisfy(== '.')) --Syntax for predicates, always end in a "."
        return(Predicate x) --Returns evaluated predicate with help of compound evaluation

-- >>> runParser predicate "edge(X,Y)."
-- [(Predicate [Compound (Atom "edge") [Variable "X",Variable "Y"]],"")]

--See, not a predicate because of the ? and lack of ","
-- >>> runParser predicate "p(zero)?"
-- []
{--------------------------------------------------------------------------}
--Rule Parser
rule  :: Parser Complex
rule = do
        x <- compound --First element is a compound
        xs <- token(string ":-") --Separated by :-
        xss <- sepBy1(symbol ",")compound --Takes another compound and separates the . by "," as seen below
        return(Rule x xss) --Returns first compound and second compound, syntax is added in printComplex

-- >>> runParser rule "p(b) :- p(a)."
-- [(Rule (Compound (Atom "p") [Atom "b"]) [Compound (Atom "p") [Atom "a"]],".")]

{--------------------------------------------------------------------------}
--Query Parser
query :: Parser Complex
query = do
        x <- sepBy1(symbol ",")compound --Takes another compound and separates the ? by "," as seen below
        punctuation <- token(satisfy(== '?')) --Syntax for predicates, always end in a "?"
        return(Query x) --Returns evaluated query with help of compound evaluation

--As seen above, query is also just another part of a whole that it takes to evaluate this input
-- >>> runParser query "p(a)? p(b)? p(c)? p(d)? p(e)? p(f)?"
-- [(Query [Compound (Atom "p") [Atom "a"]]," p(b)? p(c)? p(d)? p(e)? p(f)?")]

-- >>> runParser query "p(a)?"
-- [(Query [Compound (Atom "p") [Atom "a"]],"")]
{--------------------------------------------------------------------------}
--Syntactic Sugar Functions
sugarInt :: Int -> AST
sugarInt 0 = Atom "zero" --Base case for fully simplified value, always zero
sugarInt x = Compound(Atom "succ")[sugarInt (x-1)] --Evaluates recursively, 6, x-1, 5, etc...

-- >>> sugarInt 6
-- Compound (Atom "succ") [Compound (Atom "succ") [Compound (Atom "succ") [Compound (Atom "succ") [Compound (Atom "succ") [Compound (Atom "succ") [Atom "zero"]]]]]]

sugarList :: [AST] -> AST -> AST --Converts list AST into regular AST for handling by subParser
sugarList [] listX = listX --Base case for empty list
sugarList xs listX = sugarList(tail xs)(Compound(Atom "cons")[head xs, listX]) --Evaluates list from back to front while adding in cons syntax

{--------------------------------------------------------------------------}
--Parsing integers
intParser :: Parser AST
intParser = do
        x <- token(many1 digit) --Integer is a digit
        return(sugarInt(read(concatMap show x) :: Int)) --Processes the int and calls sugarInt to desugar it

--All integers evaluate desugared
-- >>>runParser intParser "1"
-- [(Compound (Atom "succ") [Atom "zero"],"")]

{--------------------------------------------------------------------------}
--Removing Comments  
removeComment :: Parser Complex
removeComment = do
                x <- token(sepBy(symbol "%")compound) --Comments begin with % followed by a compound
                y <- token(many1(satisfy(/= '\n'))) --Detects for newlines
                return(Comment [Atom y]) --Returns output with comment line removed

{--------------------------------------------------------------------------}
--List Parsers
listParser :: Parser AST
listParser = do
             x <- token(satisfy(== '[')) --Beginning bracket
             y <- option [](sepBy(symbol ",")subParser) --Terms defined in subparser are separated by ","
             z <- token(satisfy(== ']')) --Closing bracket
             return(sugarList(reverse y)(Atom "nil")) --returns desugared version of list y, reads back to front

-- >>> runParser listParser "[a,d]" 
-- [(Compound (Atom "cons") [Atom "a",Compound (Atom "cons") [Atom "d",Atom "nil"]],"")]

barListParser :: Parser AST
barListParser = do
             v <- token(satisfy(== '[')) --Beginning bracket
             w <- sepBy(string ",")subParser --Separates terms by "," defines them with subparser
             x <- token(satisfy(== '|')) --Bar list operator
             y <- subParser --Defines remaining terms on right side of barlist
             z <- many(token(satisfy(== ']'))) --Closing bracket
             return(sugarList(reverse w)y) --Parse lists backwards

-- >>> runParser barListParser "[1 | X]"
-- [(Compound (Atom "cons") [Compound (Atom "succ") [Atom "zero"],Variable "X"],"")]

{-------------------------------------------------------------------------}
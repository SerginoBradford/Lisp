{-# LANGUAGE FlexibleContexts #-}

{- | 
Module          : StandardFunctions
Maintainer      : Sergino Bradford GOUNOUKPEROU
Description     : Standard Functions
Copyright       : (c) Sergino Bradford, 2023

This module contains all the Standard Functions necessary for our lisp interpretor.
-}

module Lisp.Evaluation.StandardFunctions 
        (
            -- * Initial Context
            initialCtx,

            -- * Standard Functions
            define,
            quote,
            ifCm,
            lambda,
            eq,
            arithmetic,

            -- * Helper Functions
            binary,
            getSymbol,
            getSymbols
            ) where

import Lisp.Data
import Lisp.Evaluation.Eval
import qualified Data.Map as Map
import Control.Monad.State

-- | Main function for arithmetic operators.
arithmetic f  = do  (List args) <- getSymbol "..."
                    binary f args

-- | Take the Arithmetic Function, the Arguments and returning the result.
binary :: (Integer -> Integer -> Integer) -> [Expr] -> Result
binary op args = do return $ foldl1 (binaryAux op) args
	where binaryAux op (Number i) (Number j) = Number (i `op` j)

-- | Equality between Expr.
eq = do (List args) <- getSymbol "..."
        return $ foldl1 (\(Number a) (Number b) -> Number(if a == b then 1 else 0)) args

defineArgs = ["symbol", "value"]
-- | A function to modify the context.
define = do [Symbol s, e] <- getSymbols defineArgs
            eval_e <- eval e
            updateSymbolInParent s eval_e
            return eval_e

quoteArgs = ["..."]
-- | A function that return an argument without evaluate it.
quote = do  args <- getSymbols quoteArgs
            liftIO $ print args
            return (List args)

ifArgs = ["condition", "expr1", "expr2"]
-- | Conditionals.
ifCm = do
    [condExpr, expr1, expr2] <- getSymbols ifArgs
    eval_cond <- eval condExpr
    if 0 `notEqual` eval_cond then eval expr1
                                    else eval expr2
    where notEqual val1 (Number val2) = val1 /= val2

lambdaArgs = ["args", "..."]
-- | Lambda Function.
lambda = do [List args, List body] <- getSymbols lambdaArgs
            let newFn = do evalBody <- mapM eval body
                           return $ last evalBody
            return $ Function newFn (map (\(Symbol arg)->arg) args)

-- | Our Initial Context for Expr Evaluation.
initialCtx = Ctx (Map.fromList [("+", Function (arithmetic (+)) ["..."]),
			   ("-", Function (arithmetic (-)) ["..."]),
			   ("*", Function (arithmetic (*)) ["..."]),
			   ("/", Function (arithmetic div) ["..."]),
               ("eq", Function eq ["..."]),
               ("'", Special quote quoteArgs),
               ("quote", Special quote quoteArgs),
			   ("define", Special define defineArgs),
               ("if", Special ifCm ifArgs),
               ("lambda", Special lambda lambdaArgs)
			  ]) Nothing

-- | Get a symbol associated value
getSymbol sym = eval (Symbol sym)

-- | Map the getSymbol function over a list of arguments
getSymbols = mapM getSymbol
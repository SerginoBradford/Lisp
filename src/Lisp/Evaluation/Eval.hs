{-# LANGUAGE FlexibleContexts #-}

{- | 
Module          : Eval
Maintainer      : Sergino Bradford GOUNOUKPEROU
Description     : Eval function
Copyright       : (c) Sergino Bradford, 2023

This module contains the Evaluation function for treating expressions.
-}

module Lisp.Evaluation.Eval
      (
            -- * Evaluation Function
            eval,

            -- * Helper Functions
            updateSymbol,
            updateSymbolInParent,
            popContext,
            pushContext) where

import Lisp.Data
import qualified Data.Map as Map
import Data.List
import Control.Monad.State
import Control.Monad.Error

-- | Main Function for evaluating Lisp expression
eval :: Expr -> Result
eval (Number n) = return (Number n)
eval (Function f args) = return (Function f args)
eval (Special f args) = return (Special f args)
eval (Symbol s) = do
      context <- get
      lookupSymbol context
    where lookupSymbol (Ctx sym_table parentCtx) =
              if s `Map.member` sym_table
              then return (sym_table Map.! s)
              else case parentCtx of
                     Nothing -> throwError ("Symbol " ++ s ++ " is unbound.")
                     (Just parent) -> lookupSymbol parent
eval (List (x:xs)) = do
      fn <- eval x
      apply fn xs
	{-where apply (Special f expectedArgs) = apply' expectedArgs xs f
	      apply (Function f expectedArgs) = do args <- mapM eval xs
                                                   apply' expectedArgs args f
              apply' expectedArgs args f = do modify pushContext
                                              applyArgsToContext expectedArgs args
                                              result <- f
                                              modify popContext
                                              return result
              applyArgsToContext ("...":_) args = do updateSymbol "..." (List args)
              applyArgsToContext (earg:expectedArgs) (arg:args) = do updateSymbol earg arg
                                                                     applyArgsToContext expectedArgs args
              applyArgsToContext [] _ = return ()-}


apply :: Expr -> [Expr] -> Result
apply (Special f expectedArgs) list = apply' expectedArgs list f
apply (Function f expectedArgs) list = do args <- mapM eval list
                                          apply' expectedArgs args f

apply' :: [String] -> [Expr] -> Lisp.Data.Function -> Result              
apply' expectedArgs args f = do modify pushContext
                                applyArgsToContext expectedArgs args
                                result <- f
                                modify popContext
                                return result
                                where
              applyArgsToContext ("...":_) args = do updateSymbol "..." (List args)
              applyArgsToContext (earg:expectedArgs) (arg:args) = do updateSymbol earg arg
                                                                     applyArgsToContext expectedArgs args
              applyArgsToContext [] _ = return ()


-- | Update Symbol in the current Context
updateSymbol s eval_e = modify (\(Ctx sym_table parentCtx)->(Ctx (Map.insert s eval_e sym_table)) parentCtx)

-- | Update Symbol in the parent Context
updateSymbolInParent s eval_e = modify (\(Ctx sym_table parent_ctx)->(Ctx sym_table (updatedCtx parent_ctx)))
    where updatedCtx (Just (Ctx sym_table ctx)) = (Just (Ctx (Map.insert s eval_e sym_table) ctx))


-- | Create a New context and hidding the current one within it
pushContext ctx = Ctx Map.empty (Just ctx)

-- | Pop the parent Context within the current context
popContext ctx@(Ctx _ Nothing) = ctx
popContext (Ctx _ (Just parentCtx)) = parentCtx

{-# LANGUAGE FlexibleContexts #-}

{- | 
Module          : Data
Maintainer      : Sergino Bradford GOUNOUKPEROU
Description     : Data and helper functions
Copyright       : (c) Sergino Bradford, 2023

This module contains all the Data Types necessary for the mini lisp interpretor.
-}

module Lisp.Data 
    (   -- * Types
        Expr(..),
        Result(..),
        RError(..),
        Context(..),
        Function
    ) where

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Except
import qualified Control.Monad.Trans.Except as Except

-- | Our Lisp expression
data Expr = Number Integer |
            Symbol String |
            Function Function Args |
            Special Function Args |
            List [Expr]

-- The function type
type Args = [String]
type Function = Result

-- A Map holding all the symbols and the associated expr
type SymbolTable = Map.Map String Expr

-- | Context in which expressions will be evaluated
data Context = Ctx SymbolTable (Maybe Context)

-- | A Transformer monad for handling error within the context 
type RError = ErrorT String IO

-- | A state monad that holds a context and an evaluation result
type Result = StateT Context RError Expr

-- | Printing the expression
instance Show Expr where
    show (Number x) = show x
    show (Symbol x) = x
    show (Function _ _) = "<function>"
    show (Special _ _) = "<special-form-function>"
    show (List x) = "(" ++ unwords (map show x) ++ ")"

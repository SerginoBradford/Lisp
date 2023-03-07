{-# LANGUAGE FlexibleContexts #-}

--import Control.Monad.Trans.Error

{- | 
Module          : Parser
Maintainer      : Sergino Bradford GOUNOUKPEROU
Description     : Functions for expression parsing
Copyright       : (c) Sergino Bradford, 2023

This module contains all the Parsing Functions necessary for our project.
-}

module Lisp.Parser 
        (
            -- * Parse Functions
            parse,
            parseInteger,
            parseSymbol,
            parseList,
            parseExpr,

            -- * Helper Function
            parseExprAux,
            uptSrc
            ) where

import qualified Text.ParserCombinators.Parsec as P
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Except
import qualified Control.Monad.Trans.Except as Except
import Lisp.Data

-- | Function for Number parsing.
parseInteger :: P.Parser Expr
parseInteger = do sign <- P.option "" (P.string "-")
                  number <- P.many1 P.digit
                  return $ Number (read (sign++number))

-- | Function for Symbol parsing.
parseSymbol :: P.Parser Expr
parseSymbol = do f <- firstAllowed
                 r <- P.many (firstAllowed P.<|> P.digit)
                 return $ Symbol (f:r)
            where firstAllowed = P.oneOf "'+-*/" P.<|> P.letter

-- | Function for List parsing.
parseList :: P.Parser Expr
parseList = do _ <- P.char '(' ; P.skipMany P.space
               x <- parseExprAux `P.sepEndBy` (P.many1 P.space)
               _ <- P.char ')'
               return $ List x

-- | Try different parsers and return the one who succeed.
parseExprAux :: P.Parser Expr
parseExprAux = (P.try parseInteger) P.<|> (P.try parseSymbol) P.<|> (P.try parseList)

-- | Parsing the expression.
parseExpr :: P.Parser Expr
parseExpr = do P.skipMany P.space
	       x <- parseExprAux
	       P.skipMany P.space ; P.eof
	       return x

-- | Helper function for dealing with the quote function.
uptSrc :: String -> String
uptSrc [] = []
uptSrc (x:xs) = if x == '\'' then x : ' ' : uptSrc xs
                             else x : uptSrc xs  

-- | Main parsing function.
parse :: String -> Result
parse source = case P.parse parseExpr "" (uptSrc source) of
		 Right x -> return x
		 Left e -> throwError $ show e


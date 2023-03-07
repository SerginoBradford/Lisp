{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes#-}
module Main (main) where

import Lisp
import System.Environment()
import System.Exit()
import System.IO
import Control.Monad.State
import Control.Monad.Error
import Control.Monad()
import Control.Exception
import Control.Exception.Base()
import Control.Monad.Error.Class()


-- READ EVAL PRINT LOOP for the Language Interpreter.
repl :: StateT Context RError ()
repl =   do liftIO $ putStr "> "
            liftIO $ hFlush stdout
            x <- liftIO getLine

            if x == "(quit)" then return ()
                               else do
                                            readResult <- liftIO ( try (readFile x) :: IO (Either IOException String))
                                            expr <- parse ((\r src -> case r of
                                                                    (Left _) -> src
                                                                    (Right str) -> str) readResult x)
                                            evaledExpr <- eval expr
                                            liftIO $ print evaledExpr
                                            repl `catchError` (\e -> do liftIO $ putStrLn e
                                                                        repl)

-- Main function.
main :: IO ()
main = do
    result <- runErrorT (evalStateT repl initialCtx)
    case result of
        (Right _) -> print "Exiting REPL"
        (Left s) -> print s
    return ()

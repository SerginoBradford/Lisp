module Main where

import Criterion
import Criterion.Main (defaultMain)
--import System.Random
import Data.List
import Test.Hspec
import System.IO
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Error

import Lisp


main :: IO ()
main = defaultMain
    [ bgroup "Lisp tests" 
      [ bench "Simple Input Test 1" $ whnf (\str-> do 
            expr <- runErrorT (evalStateT (parse str) initialCtx)
            let expr' = case expr of
                            (Right s) ->  s
                            (Left s) ->  Symbol s
            runErrorT (evalStateT (eval expr') initialCtx)) "(* 5 5)",
        
        bench "Simple Input Test 2" $ whnf (\str-> do 
            expr <- runErrorT (evalStateT (parse str) initialCtx)
            let expr' = case expr of
                            (Right s) ->  s
                            (Left s) ->  Symbol s
            runErrorT (evalStateT (eval expr') initialCtx)) "(+ 100 (* 2 (/ 50 (- 10 5))))",
        
        bench "File Test" $ whnf (\str-> do 
            expr <- readFile str
            expr' <- runErrorT (evalStateT (parse expr) initialCtx)
            let expr'' = case expr' of
                            (Right s) ->  s
                            (Left s) ->  Symbol s
            runErrorT (evalStateT (eval expr'') initialCtx)) "lin.txt"
      ]
    ]


-- readResult <- liftIO ( try (readFile x) :: IO (Either IOException String))
--                                             expr <- parse ((\r src -> case r of
--                                                                     (Left _) -> src
--                                                                     (Right str) -> str) readResult x)
--                                             evaledExpr <- eval expr

-- main :: IO ()
-- main = do
--   [l1, l2, l3, l4, l5, l6] <- mapM 
--     randomList [1, 10, 100, 1000, 10000, 100000]

-- Generate a list of a particular size
-- randomList :: Int -> IO FenceValues
-- randomList n = FenceValues <$> (sequence $ replicate n (randomRIO (1, 10000 :: Int)))
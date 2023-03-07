{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Test.Tasty
import qualified  Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import Data.List
import Test.Hspec
import System.IO
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Error

import Lisp
import Control.Exception (evaluate)

checkResult :: Either String Expr -> String
checkResult (Left s) = show s
checkResult (Right s) = show s

spec :: Spec
spec = describe "Tests for basic functions" $ do

    describe "Parsing Tests" (do
        it "Testing Symbol error" $ do
            result <- runErrorT (evalStateT (eval (Symbol "ert")) initialCtx)
            checkResult result `shouldBe` "\"Symbol ert is unbound.\""
        
        it "Parsing 12" $ do
            expr <- runErrorT (evalStateT (parse "12") initialCtx)
            checkResult expr `shouldBe` "12"

        it "Parsing Foo" $ do
            expr <- runErrorT (evalStateT (parse "Foo") initialCtx)
            checkResult expr `shouldBe` "Foo"

        it "Parsing (+ 1 2)" $ do
            expr <- runErrorT (evalStateT (parse "(+ 1 2)") initialCtx)
            checkResult expr `shouldBe` "(+ 1 2)")
            
    describe "Arithmetics Tests" $ do
        it "Evaluate (+ 5 45)" $ do
            expr <- runErrorT (evalStateT (parse "(+ 5 45)") initialCtx)
            let expr' = case expr of
                            (Right s) ->  s
                            (Left s) ->  Symbol s
            result <- runErrorT (evalStateT (eval expr') initialCtx)
            checkResult result `shouldBe` "50"
        
        it "Evaluate (* 5 5)" $ do
            expr <- runErrorT (evalStateT (parse "(* 5 5)") initialCtx)
            let expr' = case expr of
                            (Right s) ->  s
                            (Left s) ->  Symbol s
            result <- runErrorT (evalStateT (eval expr') initialCtx)
            checkResult result `shouldBe` "25"

        it "Evaluate (/ 10 5)" $ do
            expr <- runErrorT (evalStateT (parse "(/ 10 5)") initialCtx)
            let expr' = case expr of
                            (Right s) ->  s
                            (Left s) ->  Symbol s
            result <- runErrorT (evalStateT (eval expr') initialCtx)
            checkResult result `shouldBe` "2"

        it "Evaluate (- 25 5)" $ do
            expr <- runErrorT (evalStateT (parse "(- 25 5)") initialCtx)
            let expr' = case expr of
                            (Right s) ->  s
                            (Left s) ->  Symbol s
            result <- runErrorT (evalStateT (eval expr') initialCtx)
            checkResult result `shouldBe` "20"

                    
main :: IO ()
main = hspec $ do spec

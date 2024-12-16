module Main where

import           Data.Char (isDigit)

data Expr
  = Num Int
  | Add Expr Expr
  deriving (Show)

parseExpr :: String -> Maybe Expr
parseExpr input =
  case words input of
    [x, "+", y] -> Just (Add (Num (read x)) (Num (read y)))
    [x]
      | all isDigit x -> Just (Num (read x))
    _ -> Nothing --

eval :: Expr -> Int
eval (Num n)     = n
eval (Add e1 e2) = eval e1 + eval e2

main :: IO ()
main = do
  let input = "2 + 3"
  case parseExpr input of
    Just expr -> print $ eval expr
    Nothing   -> putStrLn "Error: Invalid input"

module Main where

import Data.Char (isDigit)

data Expr
  = Num Int
  | Add Expr Expr
  | Mult Expr Expr
  deriving (Show)

parseExpr :: String -> Maybe Expr
parseExpr input =
  case words input of
    [x, "+", y] -> Just (Add (Num (read x)) (Num (read y)))
    [x]
      | all isDigit x -> Just (Num (read x))
    _ -> Nothing

main :: IO ()
main = do
  let input = "2 + 3"
  case parseExpr input of
    Just expr -> print expr
    Nothing -> putStrLn "Error: Invalid input" -- Handle error

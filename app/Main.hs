module Main where

import           Data.Char (isDigit, isSpace)

-- Define the expression data type
data Expr
  = Num Int
  | Add Expr Expr
  deriving (Show)

-- Tokenization function
tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs)
  | isSpace x = tokenize xs -- Skip whitespace
  | isDigit x =
    let (numStr, rest) = span isDigit (x : xs)
     in numStr : tokenize rest
  | x `elem` "+-*/()" = [x] : tokenize xs
  | otherwise = tokenize xs -- Ignore unexpected characters

-- Parsing function that takes a list of tokens and returns an Expr
parseExpr :: [String] -> Maybe Expr
parseExpr tokens = parseAdd tokens

-- Function to parse addition expressions
parseAdd :: [String] -> Maybe Expr
parseAdd tokens =
  case parseNum tokens of
    Just (left, ("+":rest)) ->
      case parseNum rest of
        Just (right, remainingTokens) -> Just (Add left right)
        Nothing                       -> Nothing
    Just (left, _) -> Just left
    Nothing -> Nothing

-- Function to parse numeric literals
parseNum :: [String] -> Maybe (Expr, [String])
parseNum (x:xs)
  | all isDigit x = Just (Num (read x), xs)
  | otherwise = Nothing
parseNum [] = Nothing

-- Main evaluation function for the AST
eval :: Expr -> Int
eval (Num n)     = n
eval (Add e1 e2) = eval e1 + eval e2

main :: IO ()
main = do
  let input = "4 + 5"
  let tokens = tokenize input
  putStrLn $ "Tokens: " ++ show tokens
  case parseExpr tokens of
    Just expr -> do
      putStrLn $ "Parsed Expression: " ++ show expr
      print $ eval expr
    Nothing -> putStrLn "Error: Invalid input"

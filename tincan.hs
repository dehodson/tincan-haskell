{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Text.Read
import Data.Char
import Data.Map (Map)
import System.Environment
import qualified Data.Map as Map

data Token = Dollar | At | Ampersand | Variable Char | Value Int deriving (Eq, Show)

data Program = Program { program :: [(Token, Token, Token)]
                       , len :: Int
                       , currentLine :: Int
                       , pc :: Int
                       , stack :: [Int]
                       , executionFinished :: Bool
                       , variables :: Map Char Int
                       } deriving (Show)

stripLines :: [String] -> [String]
stripLines = filter (\x -> ((==40) . length) x && (head x == '#') && (last x == '#'))

removeHash :: String -> String
removeHash = filter (/='#')

toToken :: String -> Token
toToken t = case readMaybe t :: Maybe Int of
    Just i -> Value i
    Nothing -> if (length t == 1 && (t !! 0) `elem` ['A'..'Z'])
               then Variable (t !! 0)
               else (case t of
                    "$" -> Dollar
                    "@" -> At
                    "&" -> Ampersand
                    _ -> error ("Could not parse '" ++ t ++ "'"))

clean :: T.Text -> Token
clean = toToken . T.unpack . T.strip

tokenize [x,y,z] = (clean x, clean y, clean z)
tokenize _ = error "Could not tokenize"

tokenizeLine :: String -> (Token, Token, Token)
tokenizeLine line = if (size == 3) then (tokenize split) else (error line)
    where size = length split
          split = T.splitOn "," (T.pack line)

getVariable :: Program -> Char -> Int
getVariable p = ((variables p) Map.!)

tokenToInt :: Program -> Token -> Int
tokenToInt p t = case t of
    Dollar -> currentLine p
    At -> pc p
    Ampersand -> currentLine p + 1
    Variable c -> getVariable p c
    Value i -> i 

updateVariable :: Program -> Char -> Int -> Program
updateVariable p c i = p { variables = Map.insert c i (variables p) }

jump :: Int -> Program -> Program
jump i p
    | i >= len p || i < 0 = p { currentLine = i, executionFinished = True }
    | otherwise = p { currentLine = i }

push :: Int -> Program -> Program
push i p = p { stack = i : stack p }

executeLine :: Program -> (Token, Token, Token) -> Program
executeLine p (a,(Variable b),c) = 
    if pushToStack
    then jump (currentLine p + 1) $ push diff $ updateVariable p b diff
    else if diff <= 0
         then jump address $ updateVariable p b diff
         else jump (currentLine p + 1) $ updateVariable p b diff
            where diff = value - differential
                  differential = tokenToInt p a
                  value = getVariable p b
                  pushToStack = address == -1
                  address = tokenToInt p c
executeLine _ _ = error "Could not execute line."

executeProgram :: Program -> Program
executeProgram p = executeLine p line 
    where line = program p !! currentLine p

generateProgram :: [String] -> Program
generateProgram xxs = Program (map tokenizeLine xxs) (length xxs) 0 0 [] False (Map.fromList $ zip ['A'..'Z'] (repeat 0))

main = do 
    args <- getArgs
    name <- getProgName
    case length args of
        1 -> do prog <- readFile (head args)
                putStrLn $ reverse . map chr . stack . until executionFinished executeProgram . generateProgram . map removeHash . stripLines . lines $ prog
        _ -> do putStrLn $ "Usage: " ++ name ++ " [filename]"

module Day02_test (tests) where

import Day02 (Commands (..), day02, day02p2)
import Test.HUnit
import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec

commandParser :: Parsec String u Commands
commandsParser :: Parsec String u [Commands]
commandsParser = commandParser `sepBy` newline

simpleParser :: (String, Int) -> Commands
simpleParser (str, n) = case str of
  "up " -> Up n
  "down " -> Down n
  "forward " -> Forward n
  _ -> error "Invalid input"

intLit :: Parsec String u Int
intLit = read <$> many1 digit

parseText :: Parsec String u [Char]
parseText = manyTill anyChar $ lookAhead digit

commandParser = do
  commandName <- parseText
  commandValue <- intLit
  return (simpleParser (commandName, commandValue))

testSimple :: Test
testSimple =
  TestCase $
    assertEqual
      "simple test"
      150
      (day02 [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2])

test1 :: Test
test1 =
  TestCase
    ( do
        res <- parseFromFile commandsParser "inputs/day02.txt"
        case res of
          Left err -> do
            putStrLn "Error"
            print err
          Right commandsList -> do
            assertEqual "day two part one" 1762050 (day02 commandsList)
    )

test2Simple :: Test
test2Simple =
  TestCase $
    assertEqual
      "simple test"
      900
      (day02p2 [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2])

test2 :: Test
test2 =
  TestCase
    ( do
        res <- parseFromFile commandsParser "inputs/day02.txt"
        case res of
          Left err -> do
            putStrLn "Error"
            print err
          Right commandsList -> do
            assertEqual "day two part one" 1855892637 (day02p2 commandsList)
    )

tests :: [Test]
tests = [testSimple, test1, test2Simple, test2]

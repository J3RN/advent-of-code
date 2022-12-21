-- Idea:
-- 1. Start at AA
-- 2. Try venturing to each room containing a valve with non-zero flow rate
-- 3. Open valve (update flow)
-- 4. Repeat 2–3 until all rooms with non-zero flow rate valves have been visited

import           Control.Monad (mplus)
import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Data.List (find, partition)

import           Data.Map (Map)
import qualified Data.Map as Map

type RoomName = String
type Path = [RoomName]
data Room = Room { rate :: Int,
                   peers :: [RoomName]
                 }
            deriving Show
type Graph = Map RoomName Room
data State = State { room :: Room
                   , graph :: Graph
                   , timeRemaining :: Int
                   , flow :: Int
                   }

main :: IO ()
main = putStrLn "hi"

--   let potentialNexts = Map.keys . Map.filter ((> 0) . rate) $ thisGraph

-- navigateTo :: Main.State -> RoomName -> Main.State
-- navigateTo state@(Main.State {room = thisRoom, graph = thisGraph, timeRemaining = time, flow = thisFlow}) roomName =

-- Good ole Breadth-First-Search (BFS)
pathTo :: Graph -> [RoomName] -> [Path] -> RoomName -> Maybe Path
pathTo _              _         []    _      = Nothing
pathTo thisGraph unvisitedRooms paths target =
  -- I'd prefer to use Control.Applicative's <|>, but I've imported
  -- Text.Parsec's <|> so I can't do that.
  pathTo thisGraph newUnvisited newPaths target `mplus` (fmap (drop 1 . reverse) $ find ((==) target . head) paths)
  where (newUnvisited, newPaths) = foldl foobar (unvisitedRooms, []) paths
        foobar (unvisited, np) path = let (toPrepend, stillUnvisited) = candidates unvisited (thisGraph Map.! (head path))
                                      in (stillUnvisited, map (:path) toPrepend ++ np)

candidates :: [RoomName] -> Room -> ([RoomName], [RoomName])
candidates unvisitedRooms thisRoom =
  partition (`elem` (peers thisRoom)) unvisitedRooms

-- Parser

parseFile :: Parser Graph
parseFile = Map.fromList <$> parseLine `sepEndBy1` (char '\n')

parseLine :: Parser (RoomName, Room)
parseLine = do
  _ <- string "Valve "
  name <- parseRoom
  _ <- string " has flow rate="
  thisRate <- parseRate
  _ <- string "; tunnel" *> optional (char 's') *> string " lead" *> optional (char 's') *> string " to valve" *> optional (char 's') *> char ' '
  rooms <- parseRooms
  return (name, Room {rate = thisRate, peers = rooms})

parseRate :: Parser Int
parseRate = read <$> many1 digit

parseRooms :: Parser [String]
parseRooms = parseRoom `sepBy` (string ", ")

parseRoom :: Parser String
parseRoom = many1 letter

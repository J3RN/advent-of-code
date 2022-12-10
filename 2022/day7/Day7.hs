-- Continuing with being lazy and using String

import qualified Data.Map as M

import Text.Parsec
import Text.Parsec.String (Parser)

import Text.Printf

import Debug.Trace

data Command
  = Cd { target :: String }
  | Ls { output :: [String] }
  deriving Show

-- Nodes only have names insofar as they are in a directory which has a name
-- that refers to it.  I believe this breaks from how filesystems actually work,
-- but makes the tree traversals less painful.
data Node
  = File {fileSize :: Int}
  | Dir {contents :: M.Map String Node}

type Pwd = [String]

instance Show Node where
  show (File size) = "(file, size=" ++ show size ++ ")"
  show (Dir c) = "(dir)\n" ++ (unlines . map (\l -> "  " <> l) . lines . unlines . map (\p -> (fst p) <> " " <> (show . snd $ p)) . M.toList $ c)

main :: IO ()
main = do
  fileContents <- readFile "input"
  commands <- either (fail . show) pure $ runParser parseFile () "input" fileContents
  let hier = buildHierarchy commands
      sum' = walk part1Folder 0 hier
  printf "Total of dirs of size at most 100000: %d\n" sum'
  let candidates = walk (part2Folder (30000000 - (70000000 - (nodeSize hier)))) [] hier
      smallestSufficientDirSize = minimum candidates
  printf "Smallest sufficient dir size: %d\n" smallestSufficientDirSize

part1Folder :: Int -> Node -> Int
part1Folder total (File _) = total
part1Folder total dir =
  let ns = nodeSize dir in
    if ns <= 100000
    then total + ns
    else total

part2Folder :: Int -> [Int] -> Node -> [Int]
part2Folder reqSize (candidates) dir =
  let candidateSize = nodeSize dir in
    if candidateSize >= reqSize
    then candidateSize:candidates
    else candidates

-- Convert [Command] -> Node
-- A Cd just changes the Pwd
--  Should `Cd ..` cause us to return from some kind of nested function call?
--  No! That wouldn't support cd'ing to absolute paths very well
-- An Ls adds files and subdirectories to the current Node
-- e.g. buildHeirarchy [Cd "/", Ls ["dir abc", "dir def", "123142 foo"], Cd "abc", Ls ["124214 hi"]] (Dir (fromList [("/", Dir (fromList []))])) []
buildHierarchy :: [Command] -> Node
buildHierarchy cmds = fst $ foldl updateTree (root, []) cmds
  where root = Dir {contents = M.fromList []}
        updateTree (tree, _cwd:rest) (Cd "..") = (tree, rest)
        updateTree (tree, _) (Cd "/") = (tree, [])
        updateTree (tree, pwd) (Cd dirName) = (tree, dirName:pwd)
        updateTree (tree, pwd) (Ls children) =
          let childNodes = M.fromList . map toNode $ children in
             ((addChildren (reverse pwd) childNodes tree), pwd)

toNode :: String -> (String, Node)
toNode listing =
  case words listing of
    ["dir", dirName] -> (dirName, Dir (M.fromList []))
    [fsize, fileName] -> (fileName, File (read fsize))

addChildren :: Pwd -> M.Map String Node -> Node -> Node
addChildren [] childNodes (Dir _c) =
  Dir childNodes
addChildren (name:rest) childNodes (Dir c) =
  Dir (M.adjust (addChildren rest childNodes) name c)

-- Essentially a fold, but traverses substructures
walk :: (a -> Node -> a) -> a -> Node -> a
walk fun start (Dir c) =
  foldl (walk fun) (fun start (Dir c)) c
walk fun start file =
  fun start file

nodeSize :: Node -> Int
nodeSize (File fs) = fs
nodeSize (Dir c) =
  sum . map nodeSize . M.elems $ c

-- Parser

parseFile :: Parser [Command]
parseFile = do
  many1 parseCmd

parseCmd :: Parser Command
parseCmd = do
  _ <- string "$ "
  parseCd <|> parseLs

parseCd :: Parser Command
parseCd = do
  _ <- string "cd "
  t <- many1 (alphaNum <|> (char '/') <|> (char '.'))
  _ <- char '\n'
  return (Cd t)

parseLs :: Parser Command
parseLs = do
  _ <- string "ls\n"
  c <- many dirEntries
  return (Ls c)

dirEntries :: Parser String
dirEntries = dirListing <|> fileListing

-- I wrote the parser last, and the fact that I had encapsulated some parsing in
-- toNode earlier came back to haunt me
dirListing :: Parser String
dirListing = do
  _ <- string "dir "
  dirName <- many1 alphaNum
  _ <- char '\n'
  return ("dir " <> dirName)

fileListing :: Parser String
fileListing = do
  size <- many1 digit
  _ <- char ' '
  name <- many1 (alphaNum <|> char '.')
  _ <- char '\n'
  return (size <> " " <> name)

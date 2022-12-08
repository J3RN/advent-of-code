-- Continuing with being lazy and using String

import qualified Data.Map as M

import Text.Printf

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
  deriving Show

type Pwd = [String]

main :: IO ()
main = do
  -- Parse file contents :: (String) -> [Command]
  let commands = [Cd "/", Ls ["dir abc", "dir def", "12314 foo"], Cd "abc", Ls ["12421 hi"]]
      hier = buildHeirarchy commands (Dir (M.fromList [])) []
      sum' = walk part1Folder 0 hier
  printf "Total of dirs of size at most 100000: %d\n" sum'

part1Folder :: Int -> Node -> Int
part1Folder total (File _) = total
part1Folder total dir =
  let ns = nodeSize dir in
    if ns <= 100000
    then total + ns
    else total

-- Convert [Command] -> Node
-- A Cd just changes the Pwd
--  Should `Cd ..` cause us to return from some kind of nested function call?
--  No! That wouldn't support cd'ing to absolute paths very well
-- An Ls adds files and subdirectories to the current Node
-- e.g. buildHeirarchy [Cd "/", Ls ["dir abc", "dir def", "123142 foo"], Cd "abc", Ls ["124214 hi"]] (Dir (fromList [("/", Dir (fromList []))])) []
buildHeirarchy :: [Command] -> Node -> Pwd -> Node
buildHeirarchy [] tree _pwd = tree
buildHeirarchy ((Cd ".."):rest) tree (_:upDir) =
  buildHeirarchy rest tree upDir
buildHeirarchy ((Cd "/"):rest) tree _ =
  buildHeirarchy rest tree []
buildHeirarchy ((Cd dirname):rest) tree pwd =
  buildHeirarchy rest tree (dirname:pwd)
buildHeirarchy ((Ls children):rest) tree pwd =
  let childNodes = M.fromList . map toNode $ children in
    buildHeirarchy rest (addChildren tree pwd childNodes) pwd

toNode :: String -> (String, Node)
toNode listing =
  case words listing of
    ["dir", dirName] -> (dirName, Dir (M.fromList []))
    [fsize, fileName] -> (fileName, File (read fsize))

addChildren :: Node -> Pwd -> M.Map String Node -> Node
addChildren (Dir _contents) [] childNodes =
  Dir childNodes
addChildren (Dir c) (name:rest) childNodes =
  Dir (M.insert name (addChildren (c M.! name) rest childNodes) c)

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

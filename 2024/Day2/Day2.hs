import qualified Data.List as List

main :: IO ()
main = do
  contents <- readFile "input"
  let reports = map (map (read :: String -> Int) . words) . lines $ contents
  print . numSafe $ reports
  print . numSafe' $ reports

-- A windowing function
pairs :: [a] -> [(a, a)]
pairs report = zip report (drop 1 report)

areSafeDifs :: [Int] -> Bool
areSafeDifs differences = all (\dif -> dif > 0 && dif <= 3) differences ||
                          all (\dif -> dif < 0 && dif >= -3) differences

isReportSafe :: [Int] -> Bool
isReportSafe report = let differences = map (uncurry (-)) (pairs report)
                       in areSafeDifs differences

numSafe :: [[Int]] -> Int
numSafe reports = length (filter isReportSafe reports)

dropEach :: [Int] -> [[Int]]
dropEach report = List.zipWith (++) (List.inits report) (drop 1 (List.tails report))

numSafe' :: [[Int]] -> Int
numSafe' reports = length (filter isReportSafe' reports)
  where isReportSafe' report = any isReportSafe (dropEach report)

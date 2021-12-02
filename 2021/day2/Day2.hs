module Day2 (Command, problem1, problem2) where

data Command = Forward | Up | Down

problem1 :: [(Command, Int)] -> Int
problem1 = (\(horizontal, depth) -> horizontal * depth) . foldl control (0, 0)
  where control (horizontal, depth) (Forward, amount) = (horizontal + amount, depth)
        control (horizontal, depth) (Down, amount) = (horizontal, depth + amount)
        control (horizontal, depth) (Up, amount) = (horizontal, depth - amount)

problem2 :: [(Command, Int)] -> Int
problem2 = (\(horizontal, depth, _aim) -> horizontal * depth) . foldl control (0, 0, 0)
  where control (horizontal, depth, aim) (Forward, amount) = (horizontal + amount, depth + (aim * amount), aim)
        control (horizontal, depth, aim) (Down, amount) = (horizontal, depth, aim + amount)
        control (horizontal, depth, aim) (Up, amount) = (horizontal, depth, aim - amount)

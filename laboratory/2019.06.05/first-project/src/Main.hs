module Main where

triple x = x * 3

harmonicMean activity test project = 10.0 / (c1 + c2 + c3)
    where
      c1 = 4.0 / max 0.1 activity
      c2 = 3.0 / max 0.1 test
      c3 = 3.0 / max 0.1 project

main :: IO ()
main = do
  print (triple 2)
  print (harmonicMean 10 7 8)

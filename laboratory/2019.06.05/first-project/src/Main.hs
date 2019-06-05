module Main where

triple x = x * 3

finalAverage activitie test project = 10.0 / (c1 + c2 + c3)
    where
      c1 = 4.0 / max 0.1 activitie
      c2 = 3.0 / max 0.1 test
      c3 = 3.0 / max 0.1 project

main :: IO ()
main = do
  print (triple 2)
  print (finalAverage 10 7 8)

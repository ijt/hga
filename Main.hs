import Ga

main :: IO ()
main = do
  let a = V [1,2]
      b = V [3,4]
      ab = a * b
  putStrLn $ show ab  


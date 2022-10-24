import CPUTests

import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT $ TestList [cpuTests]
  return ()

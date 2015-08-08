
import Test.Tasty


import qualified TestPlanning as TP
import TestDP as TDP


tests :: TestTree
tests = testGroup "Tests" [TP.unitTests]


main :: IO ()
main = do
  defaultMain tests
  print TDP.calcVI

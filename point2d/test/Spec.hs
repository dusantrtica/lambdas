import Test.HUnit
import Lib

-- test for quadrantPoint
test1 = TestCase (assertEqual "test1" 0 (quadrantPoint (0, 0)))

testsRequested = TestList [TestLabel "test1" test1]
main = runTestTT testsRequested

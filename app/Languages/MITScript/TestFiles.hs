module Languages.MITScript.TestFiles (
    testFiles
  , parseTestFiles
  ) where

import Term

import Languages.MITScript.Parse
import Languages.MITScript.Signature
import Languages.MITScript.Translate

--------------------------------------------

parseTestFiles :: IO [Term MITScript]
parseTestFiles = map toGeneric <$> mapM parseFile testFiles

testFiles :: [FilePath]
testFiles = [
              "MITScript/tests/basic-arithmetic.mit"
            , "MITScript/tests/basic-assignment.mit"
            , "MITScript/tests/basic-boolean.mit"
            , "MITScript/tests/basic-function.mit"
            , "MITScript/tests/basic-if.mit"
            , "MITScript/tests/basic-names.mit"
            , "MITScript/tests/basic-record.mit"
            , "MITScript/tests/basic-while.mit"
            , "MITScript/tests/good1.mit"
            , "MITScript/tests/good2.mit"
            , "MITScript/tests/good3.mit"
            , "MITScript/tests/good4.mit"
            , "MITScript/tests/good5.mit"
            , "MITScript/tests/good6.mit"
            , "MITScript/tests/good7.mit"
            , "MITScript/tests/mar5good1.mit"
            , "MITScript/tests/mar5good2.mit"
            , "MITScript/tests/mar5good3.mit"
            , "MITScript/tests/mar5good4.mit"
            , "MITScript/tests/mar5good5.mit"
            , "MITScript/tests/mar5good6.mit"
            , "MITScript/tests/mar5good7.mit"
            , "MITScript/tests/mar5test1.mit"
            , "MITScript/tests/mar5test2.mit"
            , "MITScript/tests/mar5test3.mit"
            , "MITScript/tests/mar5test4.mit"
            , "MITScript/tests/mar5test5.mit"
            , "MITScript/tests/mar5test6.mit"
            , "MITScript/tests/mar5test7.mit"
            , "MITScript/tests/mar5test8.mit"
            , "MITScript/tests/mar5test9.mit"
            , "MITScript/tests/test1.mit"
            , "MITScript/tests/test2.mit"
            , "MITScript/tests/test3.mit"
            , "MITScript/tests/test4.mit"
            , "MITScript/tests/test5.mit"
            , "MITScript/tests/test6.mit"
            , "MITScript/tests/test7.mit"
            , "MITScript/tests/test8.mit"
            , "MITScript/tests/test9.mit"
            , "MITScript/tests/worker.mit"

            , "MITScript/PerfTests/bignum.mit"
            , "MITScript/PerfTests/carsim.mit"
            , "MITScript/PerfTests/kmediods.mit"
            , "MITScript/PerfTests/life.mit"
            , "MITScript/PerfTests/textproc.mit"
            , "MITScript/PerfTests/treeproc.mit"
            ]
module Languages.Tiger.TestFiles (
   testFiles
 , parseTestFiles
 ) where

import Term

import Languages.Tiger.Parse
import Languages.Tiger.Signature
import Languages.Tiger.Translate

--------------------------------------------

parseTestFiles :: IO [Term Tiger]
parseTestFiles = map toGeneric <$> mapM parseFile testFiles

testFiles :: [FilePath]
testFiles = [
              "Tiger/testcases/merge.tig"
            , "Tiger/testcases/queens.tig"
            , "Tiger/testcases/test1.tig"
            , "Tiger/testcases/test10.tig"
            , "Tiger/testcases/test11.tig"
            , "Tiger/testcases/test12.tig"
            , "Tiger/testcases/test13.tig"
            , "Tiger/testcases/test14.tig"
            , "Tiger/testcases/test15.tig"
            , "Tiger/testcases/test16.tig"
            , "Tiger/testcases/test17.tig"
            , "Tiger/testcases/test18.tig"
            , "Tiger/testcases/test19.tig"
            , "Tiger/testcases/test2.tig"
            , "Tiger/testcases/test20.tig"
            , "Tiger/testcases/test21.tig"
            , "Tiger/testcases/test22.tig"
            , "Tiger/testcases/test23.tig"
            , "Tiger/testcases/test24.tig"
            , "Tiger/testcases/test25.tig"
            , "Tiger/testcases/test26.tig"
            , "Tiger/testcases/test27.tig"
            , "Tiger/testcases/test28.tig"
            , "Tiger/testcases/test29.tig"
            , "Tiger/testcases/test3.tig"
            , "Tiger/testcases/test30.tig"
            , "Tiger/testcases/test31.tig"
            , "Tiger/testcases/test32.tig"

            -- Invalid program
            -- , "Tiger/testcases/test33.tig"
            , "Tiger/testcases/test34.tig"
            , "Tiger/testcases/test35.tig"
            , "Tiger/testcases/test36.tig"
            , "Tiger/testcases/test37.tig"
            , "Tiger/testcases/test38.tig"
            , "Tiger/testcases/test39.tig"
            , "Tiger/testcases/test4.tig"
            , "Tiger/testcases/test40.tig"
            , "Tiger/testcases/test41.tig"
            , "Tiger/testcases/test42.tig"
            , "Tiger/testcases/test43.tig"
            , "Tiger/testcases/test44.tig"
            , "Tiger/testcases/test45.tig"
            , "Tiger/testcases/test46.tig"
            , "Tiger/testcases/test47.tig"
            , "Tiger/testcases/test48.tig"

            -- Syntax error
            --, "Tiger/testcases/test49.tig"
            , "Tiger/testcases/test5.tig"
            , "Tiger/testcases/test6.tig"
            , "Tiger/testcases/test7.tig"
            , "Tiger/testcases/test8.tig"
            , "Tiger/testcases/test9.tig"
            ]
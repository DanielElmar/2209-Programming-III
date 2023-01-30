import System.CPUTime
import Challenges

challenges :: IO ()
challenges = do
              putStr("\nStarting Testing:")
              challenge1
              challenge2
              challenge3
              challenge4
              challenge5
              challenge6
              return ()

--[]::Interactions = cast to interacton

challenge1 :: IO ()
challenge1 = do
              putStr("\n\nChallenge 1:\n\n")
              start <- getCPUTime
              test1
              test2
              test3
              test4
              test5
              test6
              test7
              end <- getCPUTime
              putStr("Challenge 1 Running Time: " ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n\n" )

challenge2 :: IO ()
challenge2 = do
              putStr("\n\nChallenge 2:\n\n")
              start <- getCPUTime
              test8
              test9
              test10
              test11
              test12
              test13
              end <- getCPUTime
              putStr("Challenge 2 Running Time: " ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n\n" )


challenge3 :: IO ()
challenge3 = do
              putStr("\n\nChallenge 3:\n\n")
              start <- getCPUTime
              test14
              test15
              test16
              test17
              test18
              end <- getCPUTime
              putStr("Challenge 3 Running Time: " ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n\n" )


challenge4 :: IO ()
challenge4 = do
              putStr("\n\nChallenge 4:\n\n")
              start <- getCPUTime
              test19
              test20
              test21
              test22
              test23
              test24
              end <- getCPUTime
              putStr("Challenge 4 Running Time: " ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n\n" )

challenge5 :: IO ()
challenge5 = do
              putStr("\n\nChallenge 5:\n\n")
              start <- getCPUTime
              test25
              test26
              test27
              test28
              test29
              end <- getCPUTime
              putStr("Challenge 5 Running Time: " ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n\n" )



challenge6 :: IO ()
challenge6 = do
              putStr("\n\nChallenge 6:\n\n")
              start <- getCPUTime
              test30
              test31
              test32
              test33
              test34
              end <- getCPUTime
              putStr("Challenge 6 Running Time: " ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n\n" )

-- https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell

test1 :: IO ()
test1 =  do
          start <- getCPUTime
          test1'
          end <- getCPUTime
          putStr("Test 1 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test1' :: IO ()
test1'  | ans == expected = putStr ("Test 1 Passed\n")
        | otherwise = putStr ("\nTest 1 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = [((North,1),Path (West,2)),((North,2),Absorb),((North,3),Path (North,6)),((North,4),Absorb),((North,5),Path (East,5)),((North,6),Path (North,3)),((North,7),Absorb),((North,8),Path (East,2)),((East,1),Path (West,1)),((East,2),Path (North,8)),((East,3),Absorb),((East,4),Path (East,7)),((East,5),Path (North,5)),((East,6),Absorb),((East,7),Path (East,4)),((East,8),Absorb),((South,1),Path (West,4)),((South,2),Absorb),((South,3),Path (West,7)),((South,4),Absorb),((South,5),Path (West,5)),((South,6),Reflect),((South,7),Absorb),((South,8),Reflect),((West,1),Path (East,1)),((West,2),Path (North,1)),((West,3),Absorb),((West,4),Path (South,1)),((West,5),Path (South,5)),((West,6),Absorb),((West,7),Path (South,3)),((West,8),Absorb)]
                ans = calcBBInteractions 8 [ (2,3) , (7,3) , (4,6) , (7,8) ]

test2 :: IO ()
test2 =  do
          start <- getCPUTime
          test2'
          end <- getCPUTime
          putStr("Test 2 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test2' :: IO ()
test2'  | ans == expected = putStr ("Test 2 Passed\n")
        | otherwise = putStr ("/nTest 2 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = [((North,1),Path (South,1)),((North,2),Path (South,2)),((North,3),Path (West,3)),((North,4),Absorb),((North,5),Absorb),((North,6),Path (East,3)),((East,1),Path (West,1)),((East,2),Path (West,2)),((East,3),Path (North,6)),((East,4),Absorb),((East,5),Absorb),((East,6),Path (South,6)),((South,1),Path (North,1)),((South,2),Path (North,2)),((South,3),Path (West,6)),((South,4),Absorb),((South,5),Absorb),((South,6),Path (East,6)),((West,1),Path (East,1)),((West,2),Path (East,2)),((West,3),Reflect),((West,4),Absorb),((West,5),Absorb),((West,6),Path (South,3))]
                ans = calcBBInteractions 6 [ (4,4) , (5,5) , (4,5) , (5,4) ]

test3 :: IO ()
test3 =  do
          start <- getCPUTime
          test3'
          end <- getCPUTime
          putStr("Test 3 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test3' :: IO ()
test3'  | ans == expected = putStr ("Test 3 Passed\n")
        | otherwise = putStr ("\nTest 3 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = [((North,1),Path (South,1)),((North,2),Path (South,2)),((North,3),Path (South,3)),((North,4),Path (South,4)),((North,5),Path (South,5)),((North,6),Path (South,6)),((North,7),Path (South,7)),((North, 8),Path (South,8)),((East,1),Path (West,1)),((East,2),Path (West,2)),((East,3),Path (West,3)),((East,4),Path (West,4)),((East,5),Path (West,5)),((East,6),Path (West,6)),((East,7),Path (West,7)),((East,8),Path (West,8)),((South,1),Path (North,1)),((South,2),Path (North,2)),((South,3),Path (North,3)),((South,4),Path (North,4)),((South,5),Path (North,5)),((South,6),Path (North,6)),((South,7),Path (North,7)),((South,8),Path (North,8)),((West,1),Path (East,1)),((West,2),Path (East,2)),((West,3),Path (East,3)),((West,4),Path (East,4)),((West,5),Path (East,5)),((West,6),Path (East,6)),((West,7) ,Path (East,7)),((West,8),Path (East,8))]
                ans = calcBBInteractions 8 [ ]

test4 :: IO ()
test4 =  do
          start <- getCPUTime
          test4'
          end <- getCPUTime
          putStr("Test 4 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test4' :: IO ()
test4'  | ans == expected = putStr ("Test 4 Passed\n")
        | otherwise = putStr ("\nTest 4 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = [((North,1),Absorb),((North,2),Reflect),((North,3),Path (South,3)),((North,4),Path (South,4)),((North,5),Path (South,5)),((North,6),Path (South,6)),((North,7),Reflect),((North,8),Absorb),((East,1),Absorb),((East,2),Reflect),((East,3),Path (West,3)),((East,4),Path (West,4)),((East,5),Path (West,5)),((East,6),Path (West,6)),((East,7),Reflect),((East,8),Absorb),((South,1),Absorb),((South,2),Reflect),((South,3),Path (North,3)),((South,4),Path (North,4)),((South,5),Path (North,5)),((South,6),Path (North,6)),((South,7),Reflect),((South,8),Absorb),((West,1),Absorb),((West,2),Path (West,2)),((West,3),Path (East,3)),((West,4),Path (East,4)),((West,5),Path (East,5)),((West,6),Path (East,6)),((West,7),Path (West,7)),((West,8),Absorb)]
                ans = calcBBInteractions 8 [ (1,1) , (1,8), (8,1), (8,8)]
test5 :: IO ()
test5 =  do
          start <- getCPUTime
          test5'
          end <- getCPUTime
          putStr("Test 5 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test5' :: IO ()
test5'  | ans == expected = putStr ("Test 5 Passed\n")
        | otherwise = putStr ("\nTest 5 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = [((North,1),Absorb),((North,2),Absorb),((North,3),Absorb),((North,4),Absorb),((North,5),Absorb),((East,1),Absorb),((East,2),Reflect),((East,3),Absorb),((East,4),Absorb),((East,5),Absorb),((South,1),Absorb),((South,2),Reflect),((South,3),Absorb),((South,4),Absorb),((South,5),Absorb),((West,1),Absorb),((West,2),Absorb),((West,3),Absorb),((West,4),Absorb),((West,5),Absorb)]
                ans = calcBBInteractions 5 [ (1,1) , (1,2) , (1,3) , (1,4) , (1,5) , (2,1) , (3,1) , (4,1) , (5,1)  ]

test6 :: IO ()
test6 =  do
          start <- getCPUTime
          test6'
          end <- getCPUTime
          putStr("Test 6 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test6' :: IO ()
test6'  | ans == expected = putStr ("Test 6 Passed\n")
        | otherwise = putStr ("\nTest 6 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = [((North,1),Path (South,1)),((North,2),Path (West,4)),((North,3),Absorb),((North,4),Reflect),((North,5),Reflect),((North,6),Absorb),((North,7),Reflect),((North,8),Absorb),((North,9),Path (East,1)),( (North,10),Path (East,13)),((North,11),Reflect),((North,12),Absorb),((North,13),Path (South,6)),((North,14),Absorb),((North,15),Path (East,6)),((East,1),Path (North,9)),((East,2),Absorb),((East,3),Absorb),((East,4),Path (West,13)),((East,5),Absorb),((East,6),Path (North,15)),((East,7),Absorb),((East,8),Path (South,15)),((East,9),Path (South,13)),((East,10),Path (West,10)),((East,11),Path (West ,11)),((East,12),Path (West,12)),((East,13),Path (North,10)),((East,14),Absorb),((East,15),Path (South,10)),((South,1),Path (North,1)),((South,2),Path (West,9)),((South,3),Absorb),((South,4),Path (South,11)),((South,5),Absorb),((South,6),Path (North,13)),((South,7),Absorb),((South,8),Path (West,15)),((South,9),Absorb),((South,10),Path (East,15)),((South,11),Path (South,4)),((South,12),Absorb), ((South,13),Path (East,9)),((South,14),Absorb),((South,15),Path (East,8)),((West,1),Absorb),((West,2),Absorb),((West,3),Absorb),((West,4),Path (North,2)),((West,5),Absorb),((West,6),Path (West,7)),( (West,7),Path (West,6)),((West,8),Absorb),((West,9),Path (South,2)),((West,10),Path (East,10)),((West,11),Path (East,11)),((West,12),Path (East,12)),((West,13),Path (East,4)),((West,14),Absorb),((West,15),Path (South,8))]
                ans = calcBBInteractions 15 [ (3,5), (8,2), (12,8), (14,7), (5,5), (6,1), (9,14), (7,3), (3,8)]



test7 :: IO ()
test7 =  do
    start <- getCPUTime
    test7'
    end <- getCPUTime
    putStr("Test 7 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test7' :: IO ()
test7'  | ans == expected = putStr ("Test 7 Passed\n")
        | otherwise = putStr ("\nTest 7 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = [((North,1),Path (South,1)),((North,2),Path (West,2)),((North,3),Absorb),((North,4),Path (East,2)),((North,5),Path (South,5)),((East,1),Path (West,1)),((East,2),Path (North,4)),((East,3),Absorb),((East,4),Path (South,4)),((East,5),Path (West,5)),((South,1),Path (North,1)),((South,2),Path (West,4)),((South,3),Absorb),((South,4),Path (East,4)),((South,5),Path (North,5)),((West,1),Path (East,1)),((West,2),Reflect),((West,3),Absorb),((West,4),Path (South,2)),((West,5),Path (East,5))]
                ans = calcBBInteractions 5 [ (3,3) ]



test8 :: IO ()
test8 =  do
    start <- getCPUTime
    test8'
    end <- getCPUTime
    putStr("Test 8 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

-- Challenge 2 Tests
test8' :: IO ()
test8'  | ans == expected = putStr ("Test 8 Passed\n")
        | otherwise = putStr ("\nTest 8 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: []")
          where expected = []
                ans = solveBB 0 [((North,1),Path (South,1)),((North,2),Path (West,2)),((North,3),Absorb),((North,4),Path (East,2)),((North,5),Path (South,5)),((East,1),Path (West,1)),((East,2),Path (North,4)),((East,3),Absorb),((East,4),Path (South,4)),((East,5),Path (West,5)),((South,1),Path (North,1)),((South,2),Path (West,4)),((South,3),Absorb),((South,4),Path (East,4)),((South,5),Path (North,5)),((West,1),Path (East,1)),((West,2),Reflect),((West,3),Absorb),((West,4),Path (South,2)),((West,5),Path (East,5))]

test9 :: IO()
test9 =  do
    start <- getCPUTime
    test9'
    end <- getCPUTime
    putStr("Test 9 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test9' :: IO ()
test9'  | ans == expected = putStr ("Test 9 Passed\n")
        | otherwise = putStr ("\nTest 9 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = [(3,3)]
                ans = solveBB 1 [((North,1),Path (South,1)),((North,2),Path (West,2)),((North,3),Absorb),((North,4),Path (East,2)),((North,5),Path (South,5)),((East,1),Path (West,1)),((East,2),Path (North,4)),((East,3),Absorb),((East,4),Path (South,4)),((East,5),Path (West,5)),((South,1),Path (North,1)),((South,2),Path (West,4)),((South,3),Absorb),((South,4),Path (East,4)),((South,5),Path (North,5)),((West,1),Path (East,1)),((West,2),Reflect),((West,3),Absorb),((West,4),Path (South,2)),((West,5),Path (East,5))]


test10 :: IO()
test10 =  do
    start <- getCPUTime
    test10'
    end <- getCPUTime
    putStr("Test 10 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test10' :: IO ()
test10' | ans == expected = putStr ("Test 10 Passed\n")
        | otherwise = putStr ("\nTest 10 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected =  [(2,3),(4,6),(7,8),(7,3)]
                ans = solveBB 4 [((North,1),Path (West,2)),((North,2),Absorb),((North,3),Path (North,6)),((North,4),Absorb),((North,5),Path (East,5)),((North,6),Path (North,3)),((North,7),Absorb),((North,8),Path (East,2)),((East,1),Path (West,1)),((East,2),Path (North,8)),((East,3),Absorb),((East,4),Path (East,7)),((East,5),Path (North,5)),((East,6),Absorb),((East,7),Path (East,4)),((East,8),Absorb),((South,1),Path (West,4)),((South,2),Absorb),((South,3),Path (West,7)),((South,4),Absorb),((South,5),Path (West,5)),((South,6),Reflect),((South,7),Absorb),((South,8),Reflect),((West,1),Path (East,1)),((West,2),Path (North,1)),((West,3),Absorb),((West,4),Path (South,1)),((West,5),Path (South,5)),((West,6),Absorb),((West,7),Path (South,3)),((West,8),Absorb)]


test11 :: IO()
test11 =  do
    start <- getCPUTime
    test11'
    end <- getCPUTime
    putStr("Test 11 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test11' :: IO ()
test11' | ans == expected = putStr ("Test 11 Passed\n")
        | otherwise = putStr ("\nTest 11 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = [(6,2),(2,6),(6,4),(6,3),(1,5)]
                ans = solveBB 5 [((North,1),Absorb),((North,2),Absorb),((North,3),Path (South,5)),((North,4),Path (South,4)),((North,5),Path (West,1)),((North,6),Absorb),((East,1),Reflect),((East,2),Absorb),((East,3),Absorb),((East,4),Absorb),((East,5),Reflect),((East,6),Absorb),((South,1),Reflect),((South,2),Absorb),((South,3),Reflect),((South,4),Path (North,4)),((South,5),Path (North,3)),((South,6),Absorb),((West,1),Path (North,5)),((West,2),Absorb),((West,3),Absorb),((West,4),Path (West,4)),((West,5),Absorb),((West,6),Path (West,6))]

test12 :: IO()
test12 =  do
    start <- getCPUTime
    test12'
    end <- getCPUTime
    putStr("Test 12 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test12' :: IO ()
test12' | ans == expected = putStr ("Test 12 Passed\n")
        | otherwise = putStr ("\nTest 12 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: []\n")
          where expected =  []
                ans = solveBB 4 []


test13 :: IO()
test13 =  do
    start <- getCPUTime
    test13'
    end <- getCPUTime
    putStr("Test 13 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test13' :: IO ()
test13' | ans == expected = putStr ("Test 13 Passed\n")
        | otherwise = putStr ("\nTest 13 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = [(3,5),(8,2),(6,2),(6,1),(5,5),(6,3),(7,3),(7,2)]
                ans = solveBB 8 [((North,1),Path (South,1)),((North,2),Path (West,4)),((North,3),Absorb),((North,4),Reflect),((North,5),Reflect),((North,6),Absorb),((North,7),Reflect),((North,8),Absorb),((North,9),Path (East,1)),((North,10),Path (South,10)),((East,1),Path (North,9)),((East,2),Absorb),((East,3),Path (South,9)),((East,4),Path (South,8)),((East,5),Absorb),((East,6),Path (South,6)),((East,7),Path (West,7)),((East,8),Path (West,8)),((East,9),Path (West,9)),((East,10),Path (West,10)),((South,1),Path (North,1)),((South,2),Path (West,6)),((South,3),Absorb),((South,4),Reflect),((South,5),Absorb),((South,6),Path (East,6)),((South,7),Absorb),((South,8),Path (East,4)),((South,9),Path (East,3)),((South,10),Path (North,10)),((West,1),Absorb),((West,2),Absorb),((West,3),Absorb),((West,4),Path (North,2)),((West,5),Absorb),((West,6),Path (South,2)),((West,7),Path (East,7)),((West,8),Path (East,8)),((West,9),Path (East,9)),((West,10),Path (East,10))]

-- Challenge 3

test14 :: IO()
test14 =  do
    start <- getCPUTime
    test14'
    end <- getCPUTime
    putStr("Test 14 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test14' :: IO ()
test14' | ans == expected = putStr ("Test 14 Passed\n")
        | otherwise = putStr ("\nTest 14 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = "(\\x1 -> x1) \\x1 -> x1"
                ans = prettyPrint (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))




test15 :: IO()
test15 =  do
    start <- getCPUTime
    test15'
    end <- getCPUTime
    putStr("Test 15 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test15' :: IO ()
test15' | ans == expected = putStr ("Test 15 Passed\n")
        | otherwise = putStr ("\nTest 15 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = "\\x1 -> x1 \\x1 -> x1"
                ans = prettyPrint (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))


test16 :: IO()
test16 =  do
    start <- getCPUTime
    test16'
    end <- getCPUTime
    putStr("Test 16 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test16' :: IO ()
test16' | ans == expected = putStr ("Test 16 Passed\n")
        | otherwise = putStr ("\nTest 16 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = "x2 0"
                ans = prettyPrint (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1))))


test17 :: IO()
test17 =  do
    start <- getCPUTime
    test17'
    end <- getCPUTime
    putStr("Test 17 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test17' :: IO ()
test17' | ans == expected = putStr ("Test 15 Passed\n")
        | otherwise = putStr ("\nTest 17 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = "1"
                ans = prettyPrint (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))) )



test18 :: IO()
test18 =  do
    start <- getCPUTime
    test18'
    end <- getCPUTime
    putStr("Test 18 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test18' :: IO ()
test18' | ans == expected = putStr ("Test 18 Passed\n")
        | otherwise = putStr ("\nTest 18 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = "2"
                ans = prettyPrint (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))))  )))



--Challenge 4

test19 :: IO()
test19 =  do
    start <- getCPUTime
    test19'
    end <- getCPUTime
    putStr("Test 19 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test19' :: IO ()
test19' | ans == expected = putStr ("Test 19 Passed\n")
        | otherwise = putStr ("\nTest 19 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: Nothing \n")
          where expected = Nothing
                ans = parseLet "let x1 = x2"




test20 :: IO()
test20 =  do
    start <- getCPUTime
    test20'
    end <- getCPUTime
    putStr("Test 20 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test20' :: IO ()
test20' | ans == expected = putStr ("Test 20 Passed\n")
        | otherwise = putStr ("\nTest 20 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = Just (LetApp (LetVar 1) (LetApp (LetVar 2) (LetVar 3)))
                ans = parseLet "x1 (x2 x3)"



test21 :: IO()
test21 =  do
    start <- getCPUTime
    test21'
    end <- getCPUTime
    putStr("Test 21 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test21' :: IO ()
test21' | ans == expected = putStr ("Test 21 Passed\n")
        | otherwise = putStr ("\nTest 21 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = Just (LetApp (LetApp (LetVar 1) (LetVar 2)) (LetVar 3))
                ans = parseLet "x1 x2 x3"


test22 :: IO()
test22 =  do
    start <- getCPUTime
    test22'
    end <- getCPUTime
    putStr("Test 22 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test22' :: IO ()
test22' | ans == expected = putStr ("Test 22 Passed\n")
        | otherwise = putStr ("\nTest 22 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = Just (LetDef [([1,1], LetVar 2)] (LetApp (LetFun 1) (LetVar 1)))
                ans = parseLet "let f1 x1 = x2 in f1 x1"



test23 :: IO()
test23 =  do
    start <- getCPUTime
    test23'
    end <- getCPUTime
    putStr("Test 23 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test23' :: IO ()
test23' | ans == expected = putStr ("Test 23 Passed\n")
        | otherwise = putStr ("\nTest 23 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = Just (LetDef [([1,2],LetVar 2),([2,1],LetVar 1)] (LetApp (LetFun 1) (LetVar 1)))
                ans = parseLet "let f1 x2 = x2; f2 x1 = x1 in f1 x1"


test24 :: IO()
test24 =  do
    start <- getCPUTime
    test24'
    end <- getCPUTime
    putStr("Test 24 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test24' :: IO ()
test24' | ans == expected = putStr ("Test 24 Passed\n")
        | otherwise = putStr ("\nTest 24 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = Just (LetVar 1)
                ans = parseLet "x1 x2 f3"


-- Challenge 5

test25 :: IO()
test25 =  do
    start <- getCPUTime
    test25'
    end <- getCPUTime
    putStr("Test 25 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test25' :: IO ()
test25' | ans == expected = putStr ("Test 25 Passed\n")
        | otherwise = putStr ("\nTest 25 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = CLApp K I
                ans = clTransform (LamAbs 1 (LamAbs 2 (LamVar 2)))


test26 :: IO()
test26 =  do
    start <- getCPUTime
    test25'
    end <- getCPUTime
    putStr("Test 26 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test26' :: IO ()
test26' | ans == expected = putStr ("Test 26 Passed\n")
        | otherwise = putStr ("\nTest 26 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = CLApp K I
                ans = clTransform (LamAbs 1 (LamAbs 2 (LamVar 2)))


test27 :: IO()
test27 =  do
    start <- getCPUTime
    test25'
    end <- getCPUTime
    putStr("Test 27 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test27' :: IO ()
test27' | ans == expected = putStr ("Test 27 Passed\n")
        | otherwise = putStr ("\nTest 27 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = CLApp (CLApp S (CLApp K (CLApp S I))) (CLApp (CLApp S (CLApp K K)) I)
                ans = clTransform (LamAbs 1 (LamAbs 2 (LamApp (LamVar 2) (LamVar 1))))


test28 :: IO()
test28 =  do
    start <- getCPUTime
    test25'
    end <- getCPUTime
    putStr("Test 28 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test28' :: IO ()
test28' | ans == expected = putStr ("Test 28 Passed\n")
        | otherwise = putStr ("\nTest 28 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = CLApp (CLApp (CLApp (CLApp S (CLApp K K)) I) (CLVar 3)) (CLApp I (CLVar 4))
                ans = clTransform (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))  (LamApp (LamAbs 1 (LamVar 1)) (LamVar 4)) )



test29 :: IO()
test29 =  do
    start <- getCPUTime
    test25'
    end <- getCPUTime
    putStr("Test 29 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test29' :: IO ()
test29' | ans == expected = putStr ("Test 29 Passed\n")
        | otherwise = putStr ("\nTest 29 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: " ++ show(expected) ++ "\n")
          where expected = CLApp (CLApp (CLApp K I) (CLApp (CLApp (CLApp S I) I) (CLApp (CLApp S I) I))) I
                ans = clTransform (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 2))) (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) )) (LamAbs 1 (LamVar 1)))


-- Challenge 6


test30 :: IO()
test30 =  do
    start <- getCPUTime
    test25'
    end <- getCPUTime
    putStr("Test 30 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test30' :: IO ()
test30' | ans == expected = putStr ("Test 30 Passed\n")
        | otherwise = putStr ("\nTest 30 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: (Just 0,Just 0,Just 0,Just 0) \n")
          where expected = (Just 0,Just 0,Just 0,Just 0)
                ans = compareInnerOuter (LamAbs 1 (LamApp (LamVar 1) (LamVar 2))) 10


test31 :: IO()
test31 =  do
    start <- getCPUTime
    test25'
    end <- getCPUTime
    putStr("Test 31 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test31' :: IO ()
test31' | ans == expected = putStr ("Test 31 Passed\n")
        | otherwise = putStr ("\nTest 31 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: (Just 1,Just 1,Just 1,Just 1) \n")
          where expected = (Just 1,Just 1,Just 1,Just 1)
                ans = compareInnerOuter (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) 10



test32 :: IO()
test32 =  do
    start <- getCPUTime
    test25'
    end <- getCPUTime
    putStr("Test 32 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test32' :: IO ()
test32' | ans == expected = putStr ("Test 32 Passed\n")
        | otherwise = putStr ("\nTest 32 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: (Nothing,Nothing,Nothing,Nothing) \n")
          where expected = (Nothing,Nothing,Nothing,Nothing)
                ans = compareInnerOuter (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))) 100



test33 :: IO()
test33 =  do
    start <- getCPUTime
    test25'
    end <- getCPUTime
    putStr("Test 33 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test33' :: IO ()
test33' | ans == expected = putStr ("Test 33 Passed\n")
        | otherwise = putStr ("\nTest 33 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: (Just 3,Just 2,Nothing,Just 4) \n")
          where expected = (Just 3,Just 2,Nothing,Just 4)
                ans = compareInnerOuter (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))  (LamApp (LamAbs 1 (LamVar 1)) (LamVar 4)) ) 4



test34 :: IO()
test34 =  do
    start <- getCPUTime
    test25'
    end <- getCPUTime
    putStr("Test 34 Running Time" ++ show((fromIntegral (end - start)) / (10^9)) ++ " ms\n" )

test34' :: IO ()
test34' | ans == expected = putStr ("Test 34 Passed\n")
        | otherwise = putStr ("\nTest 34 Failed\nActual Output: " ++ show (ans) ++ "\n\nExpected Output: (Nothing,Just 2,Nothing,Just 2) \n")
          where expected = (Nothing,Just 2,Nothing,Just 2)
                ans = compareInnerOuter (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 2))) (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) )) (LamAbs 1 (LamVar 1))) 100

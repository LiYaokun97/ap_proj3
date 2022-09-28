-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "combined-test0" $
    parseString "# hhhhh \n squares = [x*x for x in\n # sdfewfewfew \n range(10)];\n # denmark \n print([123, [squares, print(321)]]); # alsfjlekfjwel \n print('Odd squares:', [x for \n x in squares if x % 2 == 1]);\n n = 5;\n composites = [j for i in range(2, n) for j in range(i*2, n*n, i)];\n print('Printing all primes below', n*n);\n [print(x) for x in range(2,n*n) if x not in composites] \n # askflejfkwlefkwe" @?=
        Right [SDef "squares"
                    (Compr (Oper Times (Var "x") (Var "x"))
                          [CCFor "x" (Call "range" [Const (IntVal 10)])]),
              SExp (Call "print" [List [Const (IntVal 123),
                                        List [Var "squares",
                                              Call "print" [Const (IntVal 321)]]]]),
              SExp (Call "print" [Const (StringVal "Odd squares:"),
                                  Compr (Var "x") [CCFor "x" (Var "squares"),
                                                    CCIf (Oper Eq (Oper Mod (Var "x")
                                                                            (Const (IntVal 2)))
                                                                  (Const (IntVal 1)))]]),
              SDef "n" (Const (IntVal 5)),
              SDef "composites"
                    (Compr (Var "j") [CCFor "i" (Call "range" [Const (IntVal 2),Var "n"]),
                                      CCFor "j" (Call "range" [Oper Times (Var "i")
                                                                          (Const (IntVal 2)),
                                                              Oper Times (Var "n") (Var "n"),
                                                              Var "i"])]),
              SExp (Call "print" [Const (StringVal "Printing all primes below"),
                                  Oper Times (Var "n") (Var "n")]),
              SExp (Compr (Call "print" [Var "x"])
                          [CCFor "x" (Call "range" [Const (IntVal 2),
                                                    Oper Times (Var "n") (Var "n")]),
                            CCIf (Not (Oper In (Var "x") (Var "composites")))])],
    
    testCase "string-test1" $
       parseString "x = 'abas \\\n  \\\\ sdfe \\\'  \\\\  safewf ewfew 23 342 32'" @?=
        Right [SDef "x" (Const (StringVal ("abas   \\ sdfe \'  \\  safewf ewfew 23 342 32")))],
    testCase "string-test2" $
       parseString "x = 'Just a String without others'" @?=
        Right [SDef "x" (Const (StringVal ("Just a String without others")))],
    testCase "string-test3" $
       parseString "xyz = 'Just \\n a String \\\\ without others'" @?=
        Right [SDef "xyz" (Const (StringVal ("Just \n a String \\ without others")))],
    testCase "string-test4" $
      case parseString "xyz = 'Just \\\a sf a String \\\\ without others'" of
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
    testCase "string-test5" $
       parseString "x = 'Just \\n a String \\\\ without others' #sadfewf \n #safwefwfsdf " @?=
        Right [SDef "x" (Const (StringVal ("Just \n a String \\ without others")))],
    
    testCase "number-test0" $
       parseString "x = -0" @?=
        Right [SDef "x" (Const (IntVal 0))],
    testCase "number-test1" $
       parseString "x = 0" @?=
        Right [SDef "x" (Const (IntVal 0))],
    testCase "number-test2" $
       parseString "x = -100" @?=
        Right [SDef "x" (Const (IntVal (-100)))],
    testCase "number-test3" $
       case parseString "x = -100   ;\n y = 0100" of
         Left e -> return ()
         Right p ->  assertFailure $ "Unexpected parse: " ++ show p,
    testCase "number-test4" $
       case parseString "x = -100 ; # comment;\ny = 120325a #asdfwe" of
        Left e -> return ()
        Right p ->  assertFailure $ "Unexpected parse: " ++ show p,

    testCase "ident-test0" $
       parseString "_x = -0" @?=
        Right [SDef "_x" (Const (IntVal 0))],
    testCase "ident-test1" $
       parseString "sdfwef_x = 'sadfwef'" @?=
        Right [SDef "sdfwef_x" (Const (StringVal "sadfwef"))],
    testCase "ident-test2" $
       parseString "fswe23fsdfx_ = -100" @?=
        Right [SDef "fswe23fsdfx_" (Const (IntVal (-100)))],
    testCase "ident-test3" $
       case parseString "xsfew r23rdsf = -100   ;\n y = 0100" of
         Left e -> return ()
         Right p ->  assertFailure $ "Unexpected parse: " ++ show p,
    testCase "ident-test4" $
       case parseString "for = -100 ; # comment;\nsfaew = 120325a #asdfwe" of
        Left e -> return ()
        Right p ->  assertFailure $ "Unexpected parse: " ++ show p,

    testCase "oper-test0" $
       parseString "x = 1 in [1,2]" @?=
        Right [SDef "x" (Oper In (Const (IntVal 1)) (List [Const (IntVal 1), Const (IntVal 2)] ))],
    testCase "oper-test1" $
      parseString "x = 1 not in [1,2]" @?=
        Right [SDef "x" (Not (Oper In (Const (IntVal 1)) (List [Const (IntVal 1), Const (IntVal 2)] )))],
    testCase "oper-test2" $
       parseString "x = 1 >= 3" @?=
        Right [SDef "x" (Not (Oper Less (Const (IntVal 1)) (Const (IntVal 3))))],
    testCase "oper-test3" $
       parseString "x = 1 <= 3" @?=
        Right [SDef "x" (Not (Oper Greater (Const (IntVal 1)) (Const (IntVal 3))))],
    testCase "oper-test4" $
       parseString "x = 1 != (3*3)" @?=
        Right [SDef "x" (Not (Oper Eq (Const (IntVal 1)) (Oper Times (Const $ IntVal 3) (Const $ IntVal 3))))],
    testCase "oper-test5" $
       parseString "x = 1 != (3*3+1)" @?=
        Right [SDef "x" (Not (Oper Eq (Const (IntVal 1)) (Oper Plus (Oper Times (Const $ IntVal 3) (Const $ IntVal 3)) (Const $ IntVal 1) )))],
    testCase "oper-test6" $
       parseString "x = 1 != (3*(3+1))" @?=
        Right [SDef "x" (Not (Oper Eq (Const (IntVal 1)) (Oper Times (Const $IntVal 3) (Oper Plus (Const $ IntVal 3) (Const $ IntVal 1)) ) ))],
    testCase "oper-test7" $
       parseString "x = 3+5 != 3*(3+1)" @?=
        Right [SDef "x" (Not (Oper Eq (Oper Plus (Const $ IntVal 3) (Const $ IntVal 5)) (Oper Times (Const $IntVal 3) (Oper Plus (Const $ IntVal 3) (Const $ IntVal 1)) ) ))] ,
    
    testCase "comment-test0" $
       parseString "x = # asfwef \n 3+5 != 3*(3+1)" @?=
        Right [SDef "x" (Not (Oper Eq (Oper Plus (Const $ IntVal 3) (Const $ IntVal 5)) (Oper Times (Const $IntVal 3) (Oper Plus (Const $ IntVal 3) (Const $ IntVal 1)) ) ))],
    testCase "comment-test1" $
       parseString "# asfwef \n x =  3+5 # asfwef \n != 3*(3+1)" @?=
        Right [SDef "x" (Not (Oper Eq (Oper Plus (Const $ IntVal 3) (Const $ IntVal 5)) (Oper Times (Const $IntVal 3) (Oper Plus (Const $ IntVal 3) (Const $ IntVal 1)) ) ))]    ,
    testCase "comment-test2" $
       parseString "x = 'abc' # sadfew " @?=
        Right [SDef "x" (Const (StringVal ("abc")))],

    testCase "list-comprehension-test0" $
          parseString "squares = [x*x for #sfde\n x in range(10) if x > 4]" @?=
            Right [SDef "squares"
                    (Compr (Oper Times (Var "x") (Var "x"))
                      [CCFor "x" (Call "range" [Const $IntVal 10]),
                      CCIf (Oper Greater (Var "x") (Const $IntVal 4)) ])],
    testCase "list-comprehension-test1" $
      parseString "_var_hhh = [x*y for #sfde\n x in range(10) for y in range(4)]" @?=
        Right [SDef "_var_hhh"
                (Compr (Oper Times (Var "x") (Var "y"))
                  [CCFor "x" (Call "range" [Const $IntVal 10]),
                  CCFor "y" (Call "range" [Const $IntVal 4]) ])],
    testCase "list-comprehension-test2" $
      parseString "v = 100;\n_var_hhh = [x*y*v for #sfde\n x in range(10) for y in range(4)]" @?=
        Right [SDef "v" (Const $IntVal 100),
              SDef "_var_hhh"
                (Compr (Oper Times (Oper Times (Var "x") (Var "y"))(Var "v"))
                  [CCFor "x" (Call "range" [Const $IntVal 10]),
                  CCFor "y" (Call "range" [Const $IntVal 4]) ])],
    testCase "list-comprehension-test3" $
      case parseString "v = 100;\n2_var_hhh = [x*y*v for #sfde\n x in range(10) for y in range(4)]" of 
        Left e -> return ()
        Right p ->  assertFailure $ "Unexpected parse: " ++ show p,
    testCase "list-comprehension-test4" $
      parseString "v = 100;\n_var_hhh = [x*y*v for #sfde\n x in [1,2] for y in range(4)]" @?=
        Right [SDef "v" (Const $IntVal 100),
              SDef "_var_hhh"
                (Compr (Oper Times (Oper Times (Var "x") (Var "y"))(Var "v"))
                  [CCFor "x" (List [Const $ IntVal 1, Const $ IntVal 2]),
                  CCFor "y" (Call "range" [Const $IntVal 4]) ])],
    testCase "list-comprehension-test4" $
      parseString "v = 100;\n_var_hhh = [x*y*v for #sfde\n x in [x for x in range(5)] for y in range(4)]" @?=
        Right [SDef "v" (Const $IntVal 100),
              SDef "_var_hhh"
                (Compr (Oper Times (Oper Times (Var "x") (Var "y"))(Var "v"))
                  [CCFor "x" (
                    Compr (Var "x") [CCFor "x" (Call "range" [Const $ IntVal 5])]
                  ),
                  CCFor "y" (Call "range" [Const $IntVal 4]) ])]
      ]

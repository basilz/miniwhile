{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.Foldable (for_)
import qualified Data.Map as M
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import MiniWhile
import qualified MiniWhile as MW
import Test.Hspec
import Text.Parsec (parse)
import TextShow

genProgram :: Gen Program
genProgram = Program <$> genStatements

genStatements :: Gen [Stmt]
genStatements = Gen.list (Range.linear 1 10) genStatement

genId :: Gen String
genId =
  Gen.filter
    ( \x ->
        x
          `notElem` [ "if",
                      "then",
                      "else",
                      "while",
                      "do",
                      "true",
                      "false",
                      "not",
                      "and",
                      "or"
                    ]
    )
    $ (:) <$> Gen.alpha <*> Gen.string (Range.linear 0 5) Gen.alphaNum

genOp :: Gen MW.Op
genOp = Gen.element [TAdd, TMult, TDiv, TSub]

genCmpOp :: Gen CmpOp
genCmpOp = Gen.element [TLE, TGT, TEQ, TNEQ]

genAexp :: Gen Aexp
genAexp =
  Gen.recursive
    Gen.choice
    [Num <$> Gen.integral (Range.linear 1 100), MW.Var <$> genId]
    [Brk <$> genOp <*> genAexp <*> genAexp]

genExp :: Gen Exp
genExp =
  Gen.recursive
    Gen.choice
    [Aexp <$> genAexp]
    [ If <$> genExp <*> genExp <*> genExp,
      Cmp <$> genCmpOp <*> genAexp <*> genAexp,
      Not <$> genExp
    ]

genStatement :: Gen Stmt
genStatement =
  Gen.frequency
    [ (95, Asgn <$> genId <*> genExp),
      (5, While <$> genExp <*> genStatements)
    ]

prop_tripping :: Property
prop_tripping = property $ do
  p <- forAll genProgram
  tripping p (toString . showb) (parse program "")

main :: IO ()
main = do 
  x <- checkParallel $ Group "Test.Tripping" [("prop_tripping", prop_tripping) ]
  print x

-- main :: IO ()
-- main = hspec $ do
--   describe "MiniWhile" $ for_ cases _test
--   where
--     _test (input, expected) = it description assertion
--       where
--         description = "parse " <> show input
--         assertion = parseEval input >>= flip shouldBe expected

--     cases =
--       [ ("x := 3", Right (M.singleton "x" 3)),
--         ("x := 3;", Right (M.singleton "x" 3)),
--         ("x := 2; x := if x <= 3 then 3 else 1 fi", Right (M.singleton "x" 3)),
--         ("x := if 5 <= 3 then 3 else 1 fi", Right (M.singleton "x" 1)),
--         ("x := 100; while x > 3 do x := x - 1 done", Right (M.singleton "x" 3)),
--         ("x := 100; while not x <= 3 do x := x - 1 done", Right (M.singleton "x" 3)),
--         ( "x:= 0; y:= 5;while x <= 3 do y:= (y * 5); x:= (x + 1) done; y:= if y > 10000 then 10000 else y fi",
--           Right (M.fromList [("x", 4), ("y", 3125)])
--         ),
--         ( "aa := if 1 then 1 else HQ <= ( (RN +  (3 + Md) )  / 8)  fi ",
--           Left VariableNotDefined
--         ),
--         ( "aa := if 1 then 1 else HQ <= (a + b) fi ",
--           Left VariableNotDefined
--         )
--       ]

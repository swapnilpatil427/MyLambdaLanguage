import qualified ParsecParser
import Eval

test f = do
    x <- readFile f
    --putStr x
--    x <- readFile f
 -- 	putStrLn x
    putStrLn $ "***Testing " ++ x
    let ast = ParsecParser.parseExpr x
    putStrLn $ "AST: " ++ (show ast)
  --putStrLn $ "Source: " ++ (source ast)
    putStrLn $ "Eval: " ++ (show (eval (App (App (Abs "x" (Abs "y" (Binop Mul (Var "x") (Var "y")))) (Const 3)) (Const 5))))
    --putStrLn $ "Eval: " ++ (show (eval (App (Abs "y" (Binop Mul (Var "y") (Var "y"))) (Const 3))))

main = do
    test "examples/ex1.lam"
    test "examples/ex2.lam"
    test "examples/ex3.lam"
    test "examples/ex4.lam"
    test "examples/ex5.lam"
    test "examples/ex6.lam"
    --putStr x

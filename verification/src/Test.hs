import Control.Applicative
import Control.Monad ( join )
import Data.Maybe
import qualified Data.Traversable as T
import Debug.Trace
import Z3.Monad

script :: Z3 (Result, String)
script = do
  _1 <- mkInteger 1
  _3 <- mkInteger 3
  _5 <- mkInteger 5
  int_3_le_1 <- mkLe _3 _1
  int_5_le_1 <- mkLe _5 _1

  and_expression <- mkAnd [int_3_le_1, int_5_le_1]
  z3_assert <- assert and_expression
  z3_result <- check
  z3_string <- solverToString
  return (z3_result, z3_string)

main :: IO ()
main = evalZ3 script >>= \(mbSol, info) ->
    case (mbSol, info) of
        (Unsat, _)  -> putStr "Not satisfiable:" >> putStr info
        (Sat , _) -> putStr "Satisfiable :" >> putStr info
        (Undef, _) -> error "Undefineable:" >> putStr info
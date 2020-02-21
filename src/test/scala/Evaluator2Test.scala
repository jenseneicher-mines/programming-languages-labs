import org.scalatest.FlatSpec
import Ast._
import Evaluator2._

// unit tests for the Tree functions
class Evaluator2Test extends FlatSpec {
  "Eval" should "perform 3 + (4 + 5) --> 12" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === Some(v(12f)))
  }

  "Typecheck" should "typecheck the expression 3 + (4 + 5) as Number" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === Some(NumType))
  }

  it should "fail to typecheck the expression 3 + (true + 5)" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3),PlusBop,BopExpr(ve(true),PlusBop,ve(5)))) === None)
  }
}

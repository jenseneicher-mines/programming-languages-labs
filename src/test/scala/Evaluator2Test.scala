import org.scalatest.FlatSpec
import Ast._
import Evaluator2._

// unit tests for the Tree functions
class Evaluator2Test extends FlatSpec {

  //Eval Tests
  "Eval" should "perform 3 + (4 + 5) --> 12" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === Some(v(12f)))
  }
  "Eval" should "perform 3 - (1 + 1) --> 1" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),MinusBop,BopExpr(ve(1f),PlusBop,ve(1f)))) === Some(v(1f)))
  }
  "Eval" should "perform 3 + (4 - 5) --> 2" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),MinusBop,ve(5f)))) === Some(v(2f)))
  }
  "Eval" should "perform 3 - (4 - 5) --> 4" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),MinusBop,BopExpr(ve(4f),MinusBop,ve(5f)))) === Some(v(4f)))
  }
  "Eval" should "perform 3 - (4 * 5) --> -17" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),MinusBop,BopExpr(ve(4f),TimesBop,ve(5f)))) === Some(v(-17f)))
  }
  "Eval" should "perform 3 + (20 / 5) --> 7" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),PlusBop,BopExpr(ve(20f),DivBop,ve(5f)))) === Some(v(7f)))
  }


  // Type Check Tests
  "Typecheck" should "typecheck the expression 3 + (4 + 5) as Number" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === Some(NumType))
  }

  it should "fail to typecheck the expression 3 + (true + 5)" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3),PlusBop,BopExpr(ve(true),PlusBop,ve(5)))) === None)
  }
}

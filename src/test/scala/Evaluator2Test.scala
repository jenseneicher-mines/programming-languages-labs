import org.scalatest.FlatSpec
import Ast._
import Evaluator2._

// unit tests for the Tree functions
class Evaluator2Test extends FlatSpec {

  //Eval Tests
  "Eval" should "perform 3 + (4 + 5) --> 12" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === Some(v(12f)))
  }
  it should "perform 3 - (1 + 1) --> 1" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),MinusBop,BopExpr(ve(1f),PlusBop,ve(1f)))) === Some(v(1f)))
  }
  it should "perform 3 + (4 - 5) --> 2" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),MinusBop,ve(5f)))) === Some(v(2f)))
  }
  it should "perform 3 - (4 - 5) --> 4" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),MinusBop,BopExpr(ve(4f),MinusBop,ve(5f)))) === Some(v(4f)))
  }
  it should "perform 3 - (4 * 5) --> -17" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),MinusBop,BopExpr(ve(4f),TimesBop,ve(5f)))) === Some(v(-17f)))
  }
  it should "perform 3 + (20 / 5) --> 7" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),PlusBop,BopExpr(ve(20f),DivBop,ve(5f)))) === Some(v(7f)))
  }


  // Type Check Tests
  "Typecheck" should "typecheck the expression 3 + (4 + 5) as Number" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === Some(NumType))
  }

  it should "fail to typecheck the expression 3 + (true + 5)" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3f),PlusBop,BopExpr(ve(true),PlusBop,ve(5f)))) === None)
  }

  it should "fail to typecheck the expression true + 4" in {
    assert(Evaluator2.typecheck(BopExpr(ve(true),PlusBop,ve(4f))) === None)
  }

  it should "fail to typecheck the expression true + false" in {
    assert(Evaluator2.typecheck(BopExpr(ve(true),PlusBop,ve(false))) === None)
  }

  it should "typecheck the expression 1 + 2 as Number" in {
    assert(Evaluator2.typecheck(BopExpr(ve(1f),PlusBop,ve(2f))) === Some(NumType))
  }

  it should "typecheck the expression true || false as Boolean" in {
    assert(Evaluator2.typecheck(BopExpr(ve(true),OrBop,ve(false))) === Some(BoolType))
  }

  it should "typecheck the expression true || (1 + 2) === 3 as Boolean" in {
    assert(Evaluator2.typecheck(BopExpr(ve(true),OrBop,BopExpr(BopExpr(ve(1f),PlusBop,ve(2f)),EqBop,ve(3)))) === Some(BoolType))
  }
}

import org.scalatest.FlatSpec
import Ast._
import Evaluator3._

// unit tests for the Tree functions
class Evaluator3Test extends FlatSpec {
  "Eval" should "perform 3 + (4 + 5) --> 12" in {
    assert(Evaluator3.eval((List(),Map()), BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === v(12f))
  }

  it should "handle simple constant" in {
    assert(Evaluator3.eval((List(Map("x"->0)),Map(0->(Immutable,v(123f)))), BopExpr(VarExpr("x"),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === v(123f+4f+5f))
  }

  it should "handle if statements" in {
    assert(Evaluator3.eval((List(),Map()), IfExpr(ve(true), ve(false), ve(true))) === v(false))
  }

  it should "handle let statements" in {
    assert(Evaluator3.eval((List(),Map()), LetExpr(Immutable, "x", ve(123), BopExpr(VarExpr("x"),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f))))) === v(123f+4f+5f))
  }

  it should "handle print statements" in {
    PrintExpr(ve(123))
  }

  it should "handle arithmetic expressions" in {
    // perform 3 - (1 + 1) --> 1
    assert(Evaluator3.eval((List(),Map()), BopExpr(ve(3f),MinusBop,BopExpr(ve(1f),PlusBop,ve(1f)))) === v(1f))
    // perform 3 + (4 - 5) --> 2
    assert(Evaluator3.eval((List(),Map()), BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),MinusBop,ve(5f)))) === v(2f))
    // perform 3 - (4 - 5) --> 4
    assert(Evaluator3.eval((List(),Map()), BopExpr(ve(3f),MinusBop,BopExpr(ve(4f),MinusBop,ve(5f)))) === v(4f))
    // perform 3 - (4 * 5) --> -17
    assert(Evaluator3.eval((List(),Map()), BopExpr(ve(3f),MinusBop,BopExpr(ve(4f),TimesBop,ve(5f)))) === v(-17f))
    // perform 3 + (20 / 5) --> 7
    assert(Evaluator3.eval((List(),Map()), (BopExpr(ve(3f),PlusBop,BopExpr(ve(20f),DivBop,ve(5f))))) === v(7f))
  }

   it should "handle boolean expressions" in {
    assert(Evaluator3.eval((List(),Map()), BopExpr(BopExpr(ve(true), AndBop, ve(true)), OrBop, BopExpr(ve(true), AndBop, ve(false)))) === v(true))
  }

  it should "handle multiple if statements" in {
    assert(Evaluator3.eval((List(),Map()), IfExpr(IfExpr(IfExpr(IfExpr(ve(true), ve(false), ve(true)), ve(false), ve(true)), ve(false), ve(true)), ve(false), ve(true))) === v(true))
  }

  it should "handle complicated let statements" in {
    assert(Evaluator3.eval((List(),Map()), LetExpr(Immutable, "x", ve(123), LetExpr(Immutable, "x", ve(123), BopExpr(VarExpr("x"),PlusBop,LetExpr(Immutable, "x", ve(123), BopExpr(VarExpr("x"),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))))))) === v(255f))
  }

  
}

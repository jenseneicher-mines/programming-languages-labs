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
}

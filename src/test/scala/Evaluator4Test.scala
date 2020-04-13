import org.scalatest.FlatSpec
import Ast._
import Evaluator4._

// unit tests for the Tree functions
class Evaluator4Test extends FlatSpec {
  "Eval" should "perform 3 + (4 + 5) --> 12" in {
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === v(12f))
  }

  it should "handle simple constant" in {
    assert(Evaluator4.eval((List(Map("x"->0)),Map(0->(Immutable,NumVal(123f)))), BopExpr(VarExpr("x"),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === v(123f+4f+5f))
  }

  it should "handle simple recursive function" in {
    val n = 6;
    val e1 = CallExpr(LambdaExpr(Some("dec"),List(("x",None)),IfExpr(BopExpr(VarExpr("x"),GtBop,ve(0f)),BopExpr(VarExpr("x"),PlusBop,CallExpr(VarExpr("dec"),List(BopExpr(VarExpr("x"),MinusBop,ve(1f))))),ve(0f)),None),List(ve(n.toFloat)))
    assert(Evaluator4.eval((List(),Map()), e1) === v((0 to n).foldLeft(0f)(_.toFloat+_.toFloat)))
  }
}

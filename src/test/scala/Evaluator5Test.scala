import org.scalatest.FlatSpec
import Ast._
import Evaluator5._

// unit tests for the Tree functions
class Evaluator5Test extends FlatSpec {
  "Eval" should "perform 3 + (4 + 5) --> 12" in {
    assert(Evaluator5.eval((List(),Map()), BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === v(12f))
  }

  it should "handle simple constant" in {
    assert(Evaluator5.eval((List(Map("x"->0)),Map(0->(Immutable,NumVal(123f)))), BopExpr(VarExpr("x"),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === NumVal(123f+4f+5f))
  }

  it should "handle simple recursive function" in {
    val n = 6;
    val e1 = CallExpr(LambdaExpr(Some("dec"),List(("x",Some(NumType))),IfExpr(BopExpr(VarExpr("x"),GtBop,ve(0f)),BopExpr(VarExpr("x"),PlusBop,CallExpr(VarExpr("dec"),List(BopExpr(VarExpr("x"),MinusBop,ve(1f))))),ve(0f)),Some(NumType)),List(ve(n.toFloat)))
    assert(Evaluator5.eval((List(),Map()), e1) === NumVal((0 to n).foldLeft(0f)(_.toFloat+_.toFloat)))
  }

  it should "handle static typing" in {
    /*const x = 5;
     const f = (y) => x + y
     const x = 7;
     f(6)*/
    val e1 = LetExpr(Immutable, "x", ve(5f), LetExpr(Immutable, "f", LambdaExpr(None, List(("y",Some(NumType))), BopExpr(VarExpr("x"),PlusBop,VarExpr("y")), None), LetExpr(Immutable, "x", ve(7f), CallExpr(VarExpr("f"), List(ve(6f))))))
    //assert(Evaluator5.eval(Map(), e1) === NumVal(13f)) // dynamic typing
    assert(Evaluator5.eval((List(),Map()), e1) === NumVal(11f)) // static typing
    }

    it should "handle recursive factorial function" in {
      val n = 6;
      val e1 = CallExpr(LambdaExpr(Some("fact"),List(("x",Some(NumType))),IfExpr(BopExpr(VarExpr("x"),GtBop,ValueExpr(NumVal(0f))),BopExpr(VarExpr("x"),TimesBop,CallExpr(VarExpr("fact"),List(BopExpr(VarExpr("x"),MinusBop,ValueExpr(NumVal(1f)))))),ValueExpr(NumVal(1f))),Some(NumType)),List(ValueExpr(NumVal(n.toFloat))))
      assert(Evaluator5.eval((List(),Map()), e1) === NumVal((1 to n).foldLeft(1f)(_.toFloat*_.toFloat)))
    }

    "Typecheck" should "typecheck simple recursive function" in {
      val n = 6;
      val e1 = CallExpr(LambdaExpr(Some("fact"),List(("x",Some(NumType))),IfExpr(BopExpr(VarExpr("x"),GtBop,ValueExpr(NumVal(0f))),BopExpr(VarExpr("x"),TimesBop,CallExpr(VarExpr("fact"),List(BopExpr(VarExpr("x"),MinusBop,ValueExpr(NumVal(1f)))))),ValueExpr(NumVal(1f))),Some(NumType)),List(ValueExpr(NumVal(n.toFloat))))
      assert(Evaluator5.typecheck(Map(), e1) === NumType)
    }
  }

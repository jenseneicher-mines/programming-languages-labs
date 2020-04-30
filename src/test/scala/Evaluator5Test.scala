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
    val e1 = LetExpr(Immutable, "x", ve(5f), LetExpr(Immutable, "f", LambdaExpr(Some("f"), List(("y",Some(NumType))), BopExpr(VarExpr("x"),PlusBop,VarExpr("y")), None), LetExpr(Immutable, "x", ve(7f), CallExpr(VarExpr("f"), List(ve(6f))))))
    //assert(Evaluator5.eval(Map(), e1) === NumVal(13f)) // dynamic typing
    assert(Evaluator5.eval((List(),Map()), e1) === NumVal(11f)) // static typing
  }

  it should "handle recursive factorial function" in {
    val n = 6;
    val e1 = CallExpr(LambdaExpr(Some("fact"),List(("x",Some(NumType))),IfExpr(BopExpr(VarExpr("x"),GtBop,ValueExpr(NumVal(0f))),BopExpr(VarExpr("x"),TimesBop,CallExpr(VarExpr("fact"),List(BopExpr(VarExpr("x"),MinusBop,ValueExpr(NumVal(1f)))))),ValueExpr(NumVal(1f))),Some(NumType)),List(ValueExpr(NumVal(n.toFloat))))
    assert(Evaluator5.eval((List(),Map()), e1) === NumVal((1 to n).foldLeft(1f)(_.toFloat*_.toFloat)))
  }

  it should "handke a string appending function" in {
     assert(Evaluator5.eval((List(),Map()), CallExpr(LambdaExpr(Some("conc"), List(("x", None)), BopExpr(VarExpr("x"), PlusBop, ve("languages")), None), List(ve("programming")))) === v("programminglanguages"))
  }

  it should "handle a division function" in {
    assert(Evaluator5.eval((List(),Map()), CallExpr(LambdaExpr(Some("mult"), List(("x", None)), BopExpr(VarExpr("x"), DivBop, ve(5f)), None), List(ve(15f)))) === v(3f))
  }

  it should "handle a boolean function" in {
    assert(Evaluator5.eval((List(),Map()), IfExpr(ve(false),ve(true),ve(true))) === BoolVal(true))
  }

  it should "handle a squaring function" in {
    assert(Evaluator5.eval((List(),Map()), CallExpr(LambdaExpr(Some("square"), List(("x", None)), BopExpr(VarExpr("x"), TimesBop, VarExpr("x")), None), List(ve(15f)))) === v(225f))
  }

  it should "handle recursive counting function" in {
    val n = 6;
    val e1 = CallExpr(LambdaExpr(Some("count"),List(("x",Some(NumType))),IfExpr(BopExpr(VarExpr("x"),GtBop,ValueExpr(NumVal(0f))),BopExpr(VarExpr("x"),PlusBop,CallExpr(VarExpr("count"),List(BopExpr(VarExpr("x"),MinusBop,ValueExpr(NumVal(1f)))))),ValueExpr(NumVal(1f))),Some(NumType)),List(ValueExpr(NumVal(n.toFloat))))
    assert(Evaluator5.eval((List(),Map()), e1) === NumVal(22f))
  }


  "Typecheck" should "typecheck simple recursive function" in {
    val n = 6;
    val e1 = CallExpr(LambdaExpr(Some("fact"),List(("x",Some(NumType))),IfExpr(BopExpr(VarExpr("x"),GtBop,ValueExpr(NumVal(0f))),BopExpr(VarExpr("x"),TimesBop,CallExpr(VarExpr("fact"),List(BopExpr(VarExpr("x"),MinusBop,ValueExpr(NumVal(1f)))))),ValueExpr(NumVal(1f))),Some(NumType)),List(ValueExpr(NumVal(n.toFloat))))
    assert(Evaluator5.typecheck(Map(), e1) === NumType)
  }

  it should "typecheck simple arithmetic (+ - * /) statements" in {
    // + -
    // Num + Num + Num
    assert(Evaluator5.typecheck(Map(), BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === NumType)
    // Bool + Num - Num
    assert(Evaluator5.typecheck(Map(), BopExpr(ve(true),PlusBop,BopExpr(ve(4f),MinusBop,ve(5f)))) === NumType)

    // String + Num + Num
    assert(Evaluator5.typecheck(Map(), BopExpr(ve("yup"),PlusBop,BopExpr(ve(4f),MinusBop,ve(5f)))) === StringType)
    // String + Num - Bool
    assert(Evaluator5.typecheck(Map(), BopExpr(ve("yup"),PlusBop,BopExpr(ve(4f),MinusBop,ve(true)))) === StringType)
    // String + Bool
    assert(Evaluator5.typecheck(Map(), BopExpr(ve("yup"),PlusBop,ve(false))) === StringType)
    
    // * /
    // Num * Num / Num
    assert(Evaluator5.typecheck(Map(), BopExpr(ve(3f),TimesBop,BopExpr(ve(4f),DivBop,ve(5f)))) === NumType)
    // Bool * Num / Num
    assert(Evaluator5.typecheck(Map(), BopExpr(ve(true),TimesBop,BopExpr(ve(4f),DivBop,ve(5f)))) === NumType)

    // String * Num / Num
    assert(Evaluator5.typecheck(Map(), BopExpr(ve("yup"),TimesBop,BopExpr(ve(4f),DivBop,ve(5f)))) === NumType)
    // String / Num * Bool
    assert(Evaluator5.typecheck(Map(), BopExpr(ve("yup"),DivBop,BopExpr(ve(4f),TimesBop,ve(true)))) === NumType)
    // String / Bool
    assert(Evaluator5.typecheck(Map(), BopExpr(ve("yup"),DivBop,ve(false))) === NumType)
  }

  it should "throw StaticTypeErrors when it cannot statically type the statement" in {
    // String && Bool
    intercept[StaticTypeError] {
      Evaluator5.typecheck(Map(), BopExpr(ve("yup"),AndBop,ve(false)))
    }
    // String && Num
    intercept[StaticTypeError] {
      Evaluator5.typecheck(Map(), BopExpr(ve("yup"),AndBop,ve(1f)))
    }
    // Bool && Num
    intercept[StaticTypeError] {
      Evaluator5.typecheck(Map(), BopExpr(ve(true),AndBop,ve(2f)))
    }
    // String || Bool
    intercept[StaticTypeError] {
      Evaluator5.typecheck(Map(), BopExpr(ve("yup"),OrBop,ve(false)))
    }
    // String || Num
    intercept[StaticTypeError] {
      Evaluator5.typecheck(Map(), BopExpr(ve("yup"),OrBop,ve(1f)))
    }
    // Bool || Num
    intercept[StaticTypeError] {
      Evaluator5.typecheck(Map(), BopExpr(ve(true),OrBop,ve(2f)))
    }
    // ? String : Bool
    intercept[StaticTypeError] {
      Evaluator5.typecheck(Map(), IfExpr(ve(false),ve("yup"),ve(false)))
    }
    // ? String : Num
    intercept[StaticTypeError] {
      Evaluator5.typecheck(Map(), IfExpr(ve(false),ve("yup"),ve(1f)))
    }
    // ? Bool : Num
    intercept[StaticTypeError] {
      Evaluator5.typecheck(Map(), IfExpr(ve(false),ve(true),ve(2f)))
    }
  }

  it should "typecheck simple conditional statements" in {
    assert(Evaluator5.typecheck(Map(), IfExpr(ve(false),ve(true),ve(true))) === BoolType)
  }

  it should "typecheck let statements" in {
    assert(Evaluator5.typecheck(Map(), LetExpr(Immutable, "x", ve(123), BopExpr(VarExpr("x"),PlusBop,ve(5f)))) === NumType)
  }

  it should "typecheck simple recursive functions part 2" in {
    val e1 = CallExpr(LambdaExpr(Some("inc"), List(("x", Some(NumType))), BopExpr(VarExpr("x"), PlusBop, ve(1f)), None), List(ve(5f)))
    assert(Evaluator5.typecheck(Map(), e1) === NumType)
    val e2 = CallExpr(LambdaExpr(Some("conc"), List(("x", Some(StringType))), BopExpr(VarExpr("x"), PlusBop, ve("languages")), None), List(ve("programming")))
    assert(Evaluator5.typecheck(Map(), e2) === StringType)
    
    // Miss matched return type and body
    val e3 = CallExpr(LambdaExpr(Some("conc"), List(("x", Some(StringType))), BopExpr(VarExpr("x"), PlusBop, ve("languages")), Some(FunctionType(StringType, NumType))), List(ve("programming")))
    intercept[StaticTypeError] {
      Evaluator5.typecheck(Map(), e3)
    }
  }
}

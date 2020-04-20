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

  it should "handle a simple function" in {
    assert(Evaluator4.eval((List(),Map()), CallExpr(LambdaExpr(Some("inc"), List(("x", None)), BopExpr(VarExpr("x"), PlusBop, ve(1f)), None), List(ve(5f)))) === v(6f))
  }

  it should "handle simple recursive function" in {
    val n = 6;
    val e1 = CallExpr(LambdaExpr(Some("dec"),List(("x",None)),IfExpr(BopExpr(VarExpr("x"),GtBop,ve(0f)),BopExpr(VarExpr("x"),PlusBop,CallExpr(VarExpr("dec"),List(BopExpr(VarExpr("x"),MinusBop,ve(1f))))),ve(0f)),None),List(ve(n.toFloat)))
    assert(Evaluator4.eval((List(),Map()), e1) === v((0 to n).foldLeft(0f)(_.toFloat+_.toFloat)))
  }

  it should "handle + with mixed strings and integers" in {
    // perform 1 + 1 --> 2
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(1f),PlusBop,ve(1f))) === v(2f))
    // perform 'hello' + (' ' + 'world') --> 'hello world'
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("hello"),PlusBop,BopExpr(ve(" "),PlusBop,ve("world")))) === v("hello world"))
    // perform (1 + 1) + 'hello' --> '2.0hello'
    assert(Evaluator4.eval((List(),Map()), BopExpr(BopExpr(ve(1f), PlusBop, ve(1f)), PlusBop, ve("hello"))) === v("2.0hello"))
    // perform 'hello' + (1 + 'world') --> 'hello1.0world'
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("hello"),PlusBop,BopExpr(ve(1f), PlusBop,ve("world")))) === v("hello1.0world"))
  }

  it should "handle < and <= with mixed strings and integers" in {
    // perform 1 < 2 --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(1f),LtBop,ve(2f))) === v(true))
    // perform 1 < 0 --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(1f),LtBop,ve(0f))) === v(false))
    // perform 1 <= 1 --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(1f),LteBop,ve(1f))) === v(true))
    // perform 1 <= 0 --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(1f),LteBop,ve(0f))) === v(false))
    
    // perform 'apple' < 'banana' --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("apple"),LtBop,ve("banana"))) === v(true))
    // perform 'z' < 'a' --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("z"),LtBop,ve("a"))) === v(false))
    // perform 'a' <= 'a' --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("a"),LteBop,ve("a"))) === v(true))
    // perform 'z' <= 'a' --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("z"),LteBop,ve("a"))) === v(false))

    // perform '1' < 10 --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("1"),LtBop,ve(10f))) === v(true))
    // perform '13' < 10 --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("13"),LtBop,ve(10f))) === v(false))
    // perform '13' <= 13 --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("13"),LteBop,ve(13f))) === v(true))
    // perform 'apple' <= 13 --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("apple"),LteBop,ve(13f))) === v(false))
  }

  it should "handle > and >= with mixed strings and integers" in {
    // perform 1 > 2 --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(1f),GtBop,ve(2f))) === v(false))
    // perform 1 > 0 --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(1f),GtBop,ve(0f))) === v(true))
    // perform 1 >= 1 --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(1f),GteBop,ve(1f))) === v(true))
    // perform 1 >= 3 --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(1f),GteBop,ve(3f))) === v(false))
    
    // perform 'apple' > 'banana' --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("apple"),GtBop,ve("banana"))) === v(false))
    // perform 'z' > 'a' --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("z"),GtBop,ve("a"))) === v(true))
    // perform 'a' >= 'a' --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("a"),GteBop,ve("a"))) === v(true))
    // perform 'a' >= 'z' --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("a"),GteBop,ve("z"))) === v(false))

    // perform '1' > 10 --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("1"),GtBop,ve(10f))) === v(false))
    // perform '13' > 10 --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("13"),GtBop,ve(10f))) === v(true))
    // perform '13' >= 13 --> true
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("13"),GteBop,ve(13f))) === v(true))
    // perform 'apple' >= 13 --> false
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("apple"),GteBop,ve(13f))) === v(false))
  }
}

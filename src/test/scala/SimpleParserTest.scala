import org.scalatest.FlatSpec
import scala.collection.mutable.Stack
import scala.util.parsing.input._
import Ast._
import Evaluator2._

class SimpleParserTest extends FlatSpec {
  "Simple Parser Combinators" should "properly compute sums/products" in {
    assert(SimpleParser.parse("0.0") === ve(0.0f))
    assert(SimpleParser.parse("true") === ve(true))
    assert(SimpleParser.parse("1.0*2.0+3.0") === BopExpr(BopExpr(ve(1.0f), TimesBop, ve(2.0f)), PlusBop, ve(3.0f)))
    assert(SimpleParser.parse("1*2+3") === BopExpr(BopExpr(ve(1.0f), TimesBop, ve(2.0f)), PlusBop, ve(3.0f)))
  }

  it should "properly compute difference" in {
    assert(SimpleParser.parse("1.0*2.0-3.0") === BopExpr(BopExpr(ve(1.0f), TimesBop, ve(2.0f)), MinusBop, ve(3.0f)))
  }
  it should "properly compute division" in {
    assert(SimpleParser.parse("4.0/2.0") === BopExpr(ve(4.0f), DivBop, ve(2.0f)))
  }
  it should "properly compute AND" in {
    assert(SimpleParser.parse("true&&false") === BopExpr(ve(true), AndBop, ve(false)))
  }
  it should "properly compute OR" in {
    assert(SimpleParser.parse("true||false") === BopExpr(ve(true), OrBop, ve(false)))
  }
  it should "properly compute multiplicaiton" in {
    assert(SimpleParser.parse("4.0*2") === BopExpr(ve(4.0f), TimesBop, ve(2.0f)))
  }
  it should "properly compute addition and subtraction" in {
    assert(SimpleParser.parse("1.0+2.0-3.0") === BopExpr(ValueExpr(NumVal(1.0f)), PlusBop, BopExpr(ValueExpr(NumVal(2.0f)), MinusBop, ValueExpr(NumVal(3.0f)))))
  }
}

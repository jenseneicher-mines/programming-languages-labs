// http://henkelmann.eu/2011/01/28/an_introduction_to_scala_parser_combinators-part_2_literal_expressions
// http://kerflyn.wordpress.com/2012/08/25/playing-with-scala-parser-combinator/

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.Try
import Ast._
import Evaluator2._

// TODO: adapt this parser to work for boolean, numeric, and string values, and
// for the +, -, *, /, &&, || binary operators (see README for the grammar).
// NOTE - you only need to handle the above binary operators (not if-statements,
// variable bindings, unary operators, etc.)
// NOTE - for simplicity, you can assume that the -, || operators have the
// same precedence as +, and /, && have the same precedence as *.

// NOTE - For simplicity, this parser uses right associativity, while these
// operators normally are left-associative in JavaScript

object SimpleParser extends JavaTokenParsers {
  // complete example for expr
  def expr: Parser[Expr] = ((term ~ ("+" | "-" | "||") ~ expr) | term) ^^ {
    case((s:Expr) ~ (op:String) ~ (t:Expr)) => BopExpr(s, (op match {
      case "+" => PlusBop
      case "-" => MinusBop
      case "||" => OrBop
    }), t)
    case(s:Expr) => s
  }

  // Term 
  // Parse / && *
  def term: Parser[Expr] = ((atom ~ ("*" | "/" | "&&") ~ term) | atom) ^^ {
    case((s:Expr) ~ (op:String) ~ (t:Expr)) => BopExpr(s, (op match {
      case "*" => TimesBop
      case "/" => DivBop
      case "&&" => AndBop
    }), t)
    case(s:Expr) => s
  }

  def atom: Parser[Expr] = ("true"| "false" | floatingPointNumber)^^ {
    case("true") => ve(true)
    case("false") => ve(false)
    case(floatingPointNumber) => ve(NumVal(floatingPointNumber.toFloat))
  }

  def parse(s:String) : Expr = {
    expr(new CharSequenceReader(s)).get
  }
}

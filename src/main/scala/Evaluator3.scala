import scala.io.Source
import Ast._

object Evaluator3 {
  // eval(e) should return a *constant* expression
  // (if Node.js produces a constant expression (value) for an example JavaScript program,
  // your evaluator should produce that same value).
  def eval(env : Environment, e : Expr) : Value = {
    e match {
      // Completed example cases
      case ValueExpr(UndefVal) => UndefVal
      case BopExpr(e1,AndBop,e2) => BoolVal(toBool(eval(env, e1)) && toBool(eval(env, e2)))
      case PrintExpr(e) => {
        println(toStr(eval(env, e)))
        UndefVal
      }

      // TODO: replace wildcard with other cases: BoolVal, NumVal, ||,
      // +, -, *, /, <=, <, >=, >, ==, !=, - (unary), !, Vars, If,
      // Let.
      case _ => throw UnimplementedError(e)

      // See: Ast.readEnvironment, Ast.pushEnvironment

      // Unimplemented for this project
      case ValueExpr(StringVal(_)) => throw UnimplementedError(e)
      case ValueExpr(ClosureVal(_,_)) => throw UnimplementedError(e)
      case ValueExpr(ReferenceVal(_)) => throw UnimplementedError(e)
      case LetExpr(Mutable, v, e1, e2) => throw UnimplementedError(e)
      case LambdaExpr(name : Option[String], _, e1 : Expr, t2) => throw UnimplementedError(e)
      case CallExpr(e1 : Expr, _) => throw UnimplementedError(e)
      case ObjectExpr(_) => throw UnimplementedError(e)
      case AssignExpr(_,_,_) => throw UnimplementedError(e)
      case FieldExpr(_,_) => throw UnimplementedError(e)
      case ReturnExpr(_) => throw UnimplementedError(e)
    }
  }
}

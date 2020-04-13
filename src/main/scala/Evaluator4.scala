import scala.io.Source
import Ast._

object Evaluator4 {
  // eval(e) should return a *constant* expression (or throw a DynamicTypeError)
  // (as in Lab 2, use Node.js to check the correctness of your evaluator).
  def eval(env : Environment, e : Expr) : Value = {
    e match {
      case ValueExpr(UndefVal) => UndefVal
      case ValueExpr(BoolVal(b)) => BoolVal(b)
      case ValueExpr(NumVal(n)) => NumVal(n)
      case ValueExpr(StringVal(s)) => StringVal(s)
      case BopExpr(e1,AndBop,e2) => BoolVal(toBool(eval(env, e1)) && toBool(eval(env, e2)))
      case BopExpr(e1,OrBop,e2) => BoolVal(toBool(eval(env, e1)) || toBool(eval(env, e2)))
      case BopExpr(e1,MinusBop,e2) => NumVal(toNum(eval(env, e1)) - toNum(eval(env, e2)))
      case BopExpr(e1,TimesBop,e2) => NumVal(toNum(eval(env, e1)) * toNum(eval(env, e2)))
      case BopExpr(e1,DivBop,e2) => NumVal(toNum(eval(env, e1)) / toNum(eval(env, e2)))


      case BopExpr(e1,EqBop,e2) => BoolVal(eval(env, e1) == eval(env, e2))
      case BopExpr(e1,NeqBop,e2) => BoolVal(eval(env, e1) != eval(env, e2))
      case UopExpr(NegUop,e) => NumVal(-toNum(eval(env, e)))
      case UopExpr(NotUop,e) => BoolVal(!toBool(eval(env, e)))
      case VarExpr(x) => {
        val (_,v) = readEnvironment(env, x)
        v
      }
      case IfExpr(c, e1, e2) => if(toBool(eval(env, c))) eval(env, e1) else eval(env, e2)
      case LetExpr(Immutable, v, e1, e2) => eval(pushEnvironment(env, v, (Immutable,eval(env, e1))), e2)
      case PrintExpr(e) => {
        println(toStr(eval(env, e)))
        UndefVal
      }

      // TODO:
      // - PlusBop
      // - LteBop
      // - LtBop
      // - GteBop
      // - GtBop
      // - LambdaExpr
      // - CallExpr

      // See: Ast.ClosureVal
      // Note: Use dynamic scope for this lab 4

      case LambdaExpr(name : Option[String], (x : String, t)::Nil, e1 : Expr, rt) => {
        // TODO
        throw UnimplementedError(e)
      }
      case CallExpr(e1 : Expr, (e2 : Expr)::Nil) => {
        // TODO
        throw UnimplementedError(e)
      }
      case ValueExpr(ClosureVal(_,_)) => throw UnimplementedError(e)
      case ValueExpr(ReferenceVal(_)) => throw UnimplementedError(e)
      case LetExpr(Mutable, v, e1, e2) => throw UnimplementedError(e)
      case LambdaExpr(name : Option[String], Nil, e1 : Expr, t2) => throw UnimplementedError(e)
      case LambdaExpr(name : Option[String], a::b::more, e1 : Expr, t2) => throw UnimplementedError(e)
      case CallExpr(e1 : Expr, Nil) => throw UnimplementedError(e)
      case CallExpr(e1 : Expr, a::b::more) => throw UnimplementedError(e)
      case ObjectExpr(_) => throw UnimplementedError(e)
      case AssignExpr(_,_,_) => throw UnimplementedError(e)
      case FieldExpr(_,_) => throw UnimplementedError(e)
      case ReturnExpr(_) => throw UnimplementedError(e)
    }
  }
}

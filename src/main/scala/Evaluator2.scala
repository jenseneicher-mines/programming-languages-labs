import scala.io.Source
import Ast._

object Evaluator2 {
  // TODO: eval(e) should return a *constant* expression on success
  // (expressions such as (true + 2) that aren't able to be evaluated without implicit
  // conversions should return None to indicate failure)
  def eval(e : Expr) : Option[Value] = {
    e match {
      // Completed example cases
      case ValueExpr(BoolVal(b)) => Some(BoolVal(b))
      case BopExpr(e1,AndBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(BoolVal(b1)),Some(BoolVal(b2))) => Some(BoolVal(b1 && b2))
          case _ => None
        }
      }

      // TODO: replace wildcard with other cases
      case _ => throw UnimplementedError(e)


      // Unimplemented for this project
      case VarExpr(_) => throw UnimplementedError(e)
      case LetExpr(Immutable, _, _, _) => throw UnimplementedError(e)
      case ValueExpr(UndefVal) => throw UnimplementedError(e)
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
      case IfExpr(_, _, _) => throw UnimplementedError(e)
      case PrintExpr(_) => throw UnimplementedError(e)
    }
  }

  // TODO: on success, this function should return the type of expression e
  // (return None to indicate failure, e.g., when the expression is something
  // that's not well-typed, like (1 + true))
  def typecheck(e : Expr) : Option[Type] = {
    e match {
      case ValueExpr(BoolVal(_)) => Some(BoolType)
      case ValueExpr(NumVal(_)) => Some(NumType)
      case BopExpr(e1,(AndBop|OrBop),e2) => {
        val t1 = typecheck(e1)
        val t2 = typecheck(e2)
        (t1,t2) match {
          case (Some(BoolType),Some(BoolType)) => Some(BoolType)
          case _ => None
        }
      }
      // TODO: replace wildcard with other cases
      case _ => throw UnimplementedError(e)

      // Unimplemented for this project
      case ValueExpr(StringVal(_)) => throw UnimplementedError(e)
      case ValueExpr(UndefVal) => throw UnimplementedError(e)
      case AssignExpr(_, _, _) => throw UnimplementedError(e)
      case CallExpr(_, _) => throw UnimplementedError(e)
      case FieldExpr(_, _) => throw UnimplementedError(e)
      case IfExpr(_, _, _) => throw UnimplementedError(e)
      case LambdaExpr(_, _, _, _) => throw UnimplementedError(e)
      case LetExpr(_, _, _, _) => throw UnimplementedError(e)
      case ObjectExpr(_) => throw UnimplementedError(e)
      case PrintExpr(_) => throw UnimplementedError(e)
      case ReturnExpr(_) => throw UnimplementedError(e)
      case VarExpr(_) => throw UnimplementedError(e)
      case ValueExpr(ClosureVal(_, _)) => throw UnimplementedError(e)
      case ValueExpr(ReferenceVal(_)) => throw UnimplementedError(e)
    }
  }
}

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
      case ValueExpr(NumVal(b)) => Some(NumVal(b))

      // Binary Operators
      // &&
      case BopExpr(e1,AndBop,e2) => { 
        (eval(e1),eval(e2)) match {
          case (Some(BoolVal(b1)),Some(BoolVal(b2))) => Some(BoolVal(b1 && b2))
          case _ => None
        }
      }
      // ||
      case BopExpr(e1,OrBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(BoolVal(b1)),Some(BoolVal(b2))) => Some(BoolVal(b1 || b2))
          case _ => None
        }
      }
      // +
      case BopExpr(e1,PlusBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(NumVal(b1)),Some(NumVal(b2))) => Some(NumVal(b1 + b2))
          case _ => None
        }
      }
      // -
      case BopExpr(e1,MinusBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(NumVal(b1)),Some(NumVal(b2))) => Some(NumVal(b1 - b2))
          case _ => None
        }
      }
      // *
      case BopExpr(e1,TimesBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(NumVal(b1)),Some(NumVal(b2))) => Some(NumVal(b1 * b2))
          case _ => None
        }
      }
      // /
      case BopExpr(e1,DivBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(NumVal(b1)),Some(NumVal(b2))) => Some(NumVal(b1 / b2))
          case _ => None
        }
      }
      // ===
      case BopExpr(e1,EqBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(NumVal(b1)),Some(NumVal(b2))) => Some(BoolVal(b1 == b2))
          case (Some(BoolVal(b1)),Some(BoolVal(b2))) => Some(BoolVal(b1 == b2))
          case _ => None
        }
      }
      // !==
      case BopExpr(e1,NeqBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(NumVal(b1)),Some(NumVal(b2))) => Some(BoolVal(b1 != b2))
          case (Some(BoolVal(b1)),Some(BoolVal(b2))) => Some(BoolVal(b1 != b2))
          case _ => None
        }
      }
      // <
      case BopExpr(e1,LtBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(NumVal(b1)),Some(NumVal(b2))) => Some(BoolVal(b1 < b2))
          case (Some(BoolVal(b1)),Some(BoolVal(b2))) => Some(BoolVal(b1 < b2))
          case _ => None
        }
      }
      // <=
      case BopExpr(e1,LteBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(NumVal(b1)),Some(NumVal(b2))) => Some(BoolVal(b1 <= b2))
          case (Some(BoolVal(b1)),Some(BoolVal(b2))) => Some(BoolVal(b1 <= b2))
          case _ => None
        }
      }
      // >
      case BopExpr(e1,GtBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(NumVal(b1)),Some(NumVal(b2))) => Some(BoolVal(b1 > b2))
          case (Some(BoolVal(b1)),Some(BoolVal(b2))) => Some(BoolVal(b1 > b2))
          case _ => None
        }
      }
      // >=
      case BopExpr(e1,GteBop,e2) => {
        (eval(e1),eval(e2)) match {
          case (Some(NumVal(b1)),Some(NumVal(b2))) => Some(BoolVal(b1 >= b2))
          case (Some(BoolVal(b1)),Some(BoolVal(b2))) => Some(BoolVal(b1 >= b2))
          case _ => None
        }
      }

      // Unary Operators
      // !
      case UopExpr(NotUop,e1) => {
        eval(e1) match {
          case Some(BoolVal(b1)) => Some(BoolVal(!b1))
          case _ => None
        }
      }
      // -
      case UopExpr(NegUop,e1) => {
        eval(e1) match {
          case Some(NumVal(b1)) => Some(NumVal(-b1))
          case _ => None
        }
      }

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
      // Binary Operators
      // && ||
      case BopExpr(e1,(AndBop|OrBop),e2) => {
        val t1 = typecheck(e1)
        val t2 = typecheck(e2)
        (t1,t2) match {
          case (Some(BoolType),Some(BoolType)) => Some(BoolType)
          case _ => None
        }
      }
      // + - * /
      case BopExpr(e1,(PlusBop|MinusBop|TimesBop|DivBop),e2) => {
        val t1 = typecheck(e1)
        val t2 = typecheck(e2)
        (t1,t2) match {
          case (Some(NumType),Some(NumType)) => Some(NumType)
          case _ => None
        }
      }
      // === !=== < <= > >=
      case BopExpr(e1,(EqBop|NeqBop|LtBop|LteBop|GtBop|GteBop),e2) => {
        val t1 = typecheck(e1)
        val t2 = typecheck(e2)
        (t1,t2) match {
          case (Some(NumType),Some(NumType)) => Some(BoolType)
          case (Some(BoolType),Some(BoolType)) => Some(BoolType)
          case _ => None
        }
      }

      // Unary Operators
      // !
      case UopExpr((NotUop), e1) => {
        typecheck(e1) match {
          case Some(BoolType) => Some(BoolType)
          case _ => None
        }
      }
      // -
      case UopExpr((NegUop), e1) => {
        typecheck(e1) match {
          case Some(NumType) => Some(NumType)
          case _ => None
        }
      }

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

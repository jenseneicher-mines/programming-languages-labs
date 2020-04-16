import scala.io.Source
import Ast._

object Evaluator5 {
  def typecheck(env : Map[String,Type], e : Expr) : Type = {
    e match {
      /* Examples */
      case ValueExpr(UndefVal) => UnitType
      case ValueExpr(BoolVal(_)) => BoolType
      case ValueExpr(NumVal(_)) => NumType
      case ValueExpr(StringVal(_)) => StringType
      case BopExpr(e1,AndBop,e2) => BoolType
      case PrintExpr(e) => UnitType
      case IfExpr(c, e1, e2) => {
        (typecheck(env, e1),typecheck(env, e2)) match {
          case (t1,t2) if t1==t2 => t1
          case _ => throw StaticTypeError(e)
        }
      }

      /* TODO */
      case BopExpr(e1,OrBop,e2) => throw UnimplementedError(e)
      case BopExpr(e1,PlusBop,e2) => throw UnimplementedError(e)
      case BopExpr(e1,MinusBop,e2) => throw UnimplementedError(e)
      case BopExpr(e1,TimesBop,e2) => throw UnimplementedError(e)
      case BopExpr(e1,DivBop,e2) => throw UnimplementedError(e)
      case BopExpr(e1,LteBop,e2) => throw UnimplementedError(e)
      case BopExpr(e1,LtBop,e2) => throw UnimplementedError(e)
      case BopExpr(e1,GteBop,e2) => throw UnimplementedError(e)
      case BopExpr(e1,GtBop,e2) => throw UnimplementedError(e)
      case BopExpr(e1,EqBop,e2) => throw UnimplementedError(e)
      case BopExpr(e1,NeqBop,e2) => throw UnimplementedError(e)
      case UopExpr(NegUop,e) => throw UnimplementedError(e)
      case UopExpr(NotUop,e) => throw UnimplementedError(e)
      case VarExpr(v) => throw UnimplementedError(e)
      case LetExpr(Immutable, v, e1, e2) => throw UnimplementedError(e)
      case LambdaExpr(name : Option[String], (x : String, Some(xt : Type))::Nil, ex : Expr, ext : Option[Type]) => throw UnimplementedError(e)
      case CallExpr(e1 : Expr, (e2 : Expr)::Nil) => throw UnimplementedError(e)
      /* END OF TODO */

      /* Other Lambda */
      case LambdaExpr(name : Option[String], _, ex : Expr, ext : Option[Type]) => throw StaticTypeError(e)

      /* Unused in this project */
      case LetExpr(Mutable, _, _, _) => throw UnimplementedError(e)
      case CallExpr(e1 : Expr, Nil) => throw UnimplementedError(e)
      case CallExpr(e1 : Expr, a::b::more) => throw UnimplementedError(e)
      case ObjectExpr(_) => throw UnimplementedError(e)
      case AssignExpr(_,_,_) => throw UnimplementedError(e)
      case FieldExpr(_,_) => throw UnimplementedError(e)
      case ReturnExpr(_) => throw UnimplementedError(e)
      case ValueExpr(ClosureVal(_,_)) => throw UnimplementedError(e)
      case ValueExpr(ReferenceVal(_)) => throw UnimplementedError(e)
    }
  }

  // eval(e) should return a *constant* expression (or throw a DynamicTypeError)
  // (as in Lab 2, use Node.js to check the correctness of your evaluator).
  def eval(env : Environment, e : Expr) : Value = {
    e match {
      case ValueExpr(v) => v
      case BopExpr(e1,AndBop,e2) => BoolVal(toBool(eval(env, e1)) && toBool(eval(env, e2)))
      case BopExpr(e1,OrBop,e2) => BoolVal(toBool(eval(env, e1)) || toBool(eval(env, e2)))
      case BopExpr(e1,PlusBop,e2) => {
        (eval(env, e1), eval(env, e2)) match {
          case (StringVal(s1),v2) => StringVal(s1 + toStr(v2))
          case (v1,StringVal(s2)) => StringVal(toStr(v1) + s2)
          case (v1,v2) => NumVal(toNum(v1) + toNum(v2))
        }
      }
      case BopExpr(e1,MinusBop,e2) => NumVal(toNum(eval(env, e1)) - toNum(eval(env, e2)))
      case BopExpr(e1,TimesBop,e2) => NumVal(toNum(eval(env, e1)) * toNum(eval(env, e2)))
      case BopExpr(e1,DivBop,e2) => NumVal(toNum(eval(env, e1)) / toNum(eval(env, e2)))
      case BopExpr(e1,LteBop,e2) => {
        (eval(env, e1), eval(env, e2)) match {
          case (StringVal(s1),StringVal(s2)) => BoolVal(s1 <= s2)
          case (v1,v2) => BoolVal(toNum(v1) <= toNum(v2))
        }
      }
      case BopExpr(e1,LtBop,e2) => {
        (eval(env, e1), eval(env, e2)) match {
          case (StringVal(s1),StringVal(s2)) => BoolVal(s1 < s2)
          case (v1,v2) => BoolVal(toNum(v1) < toNum(v2))
        }
      }
      case BopExpr(e1,GteBop,e2) => {
        (eval(env, e1), eval(env, e2)) match {
          case (StringVal(s1),StringVal(s2)) => BoolVal(s1 >= s2)
          case (v1,v2) => BoolVal(toNum(v1) >= toNum(v2))
        }
      }
      case BopExpr(e1,GtBop,e2) => {
        (eval(env, e1), eval(env, e2)) match {
          case (StringVal(s1),StringVal(s2)) => BoolVal(s1 > s2)
          case (v1,v2) => BoolVal(toNum(v1) > toNum(v2))
        }
      }
      case BopExpr(e1,EqBop,e2) => BoolVal(eval(env, e1) == eval(env, e2))
      case BopExpr(e1,NeqBop,e2) => BoolVal(eval(env, e1) != eval(env, e2))
      case UopExpr(NegUop,e) => NumVal(-toNum(eval(env, e)))
      case UopExpr(NotUop,e) => BoolVal(!toBool(eval(env, e)))
      case VarExpr(x) => {
        val (_,v) = readEnvironment(env,x)
        v
      }
      case IfExpr(c, e1, e2) => if(toBool(eval(env, c))) eval(env, e1) else eval(env, e2)
      case LetExpr(Immutable, v, e1, e2) => eval(pushEnvironment(env, v, (Immutable,eval(env, e1))), e2)
      case PrintExpr(e) => {
        println(toStr(eval(env, e)))
        UndefVal
      }
      case LambdaExpr(name : Option[String], (x : String, t1)::Nil, e1 : Expr, t2) => {
        // TODO
        throw UnimplementedError(e)
      }
      case CallExpr(e1 : Expr, (e2 : Expr)::Nil) => {
        // TODO
        throw UnimplementedError(e)
      }


      /* Unused in this project */
      case LetExpr(Mutable, _, _, _) => throw UnimplementedError(e)
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

  // def main(args : Array[String]) = {
  //   args.foreach(f => {
  //     println("Processing file: "+f)
  //     val s = Source.fromFile(f).getLines.mkString
  //     println("File contents: "+s)
  //     val e = parse(s)
  //     println("Parsed: "+e)
  //     println("Formatted: "+strExpr(e))
  //     val e2 = eval((Nil,Map()), e)
  //     println("Result: "+e2)
  //   })
  // }
}

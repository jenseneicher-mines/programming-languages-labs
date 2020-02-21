// http://henkelmann.eu/2011/01/28/an_introduction_to_scala_parser_combinators-part_2_literal_expressions
// http://kerflyn.wordpress.com/2012/08/25/playing-with-scala-parser-combinator/

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.Try

object Ast {
  // binary operators
  sealed trait Bop
  case object AndBop extends Bop // "&&" operator
  case object OrBop extends Bop  // "||" operator
  case object PlusBop extends Bop  // "+" operator
  case object MinusBop extends Bop // "-" operator
  case object TimesBop extends Bop // "*" operator
  case object DivBop extends Bop   // "/" operator
  case object EqBop extends Bop   // "===" operator
  case object NeqBop extends Bop   // "!==" operator
  case object LtBop extends Bop   // "<" operator
  case object LteBop extends Bop   // "<=" operator
  case object GtBop extends Bop   // ">" operator
  case object GteBop extends Bop   // ">=" operator

  // unary operators
  sealed trait Uop
  case object NotUop extends Uop // "!" operator
  case object NegUop extends Uop // "-" operator

  type NameMap = Map[String,Int]
  type IdentMap = Map[Int,(VarAccess,Value)]
  type Environment = (List[NameMap],IdentMap)
    type HeapObject = List[(String,Value)]
  type Heap = List[HeapObject]

  var uniqueId = 1

  sealed trait VarAccess
  case object Mutable extends VarAccess
  case object Immutable extends VarAccess

  type TypedVar = (String,Option[Type])

    // expressions
    sealed trait Value

  case class  NumVal(f : Float) extends Value                      // JavaScript float value, e.g., 123.45
  case class  BoolVal(b : Boolean) extends Value                   // JavaScript bool value, e.g., true, false
  case class  StringVal(s : String) extends Value                  // JavaScript string value, e.g., "some string"
  case object UndefVal extends Value                               // JavaScript undef value, e.g., undefined
  case class  ClosureVal(m: NameMap, l : LambdaExpr) extends Value // closure (define-time environment and lambda)
  case class  ReferenceVal(address : Int) extends Value            // reference

  sealed trait Expr

  case class  ValueExpr(v : Value) extends Expr
  case class  BopExpr(e1 : Expr, op : Bop, e2 : Expr) extends Expr                   // e1 + e2
  case class  UopExpr(op : Uop, e1 : Expr) extends Expr                              // !e
  case class  VarExpr(x : String) extends Expr                                       // x
  case class  IfExpr(c : Expr, e1 : Expr, e2 : Expr) extends Expr                    // c ? e1 : e2
  case class  LetExpr(lt : VarAccess, x : String, e1 : Expr, e2 : Expr) extends Expr // const v = e1; e2
  case class  PrintExpr(e1 : Expr) extends Expr                                      // console.log(e1)

  case class  LambdaExpr(name : Option[String], xs : List[TypedVar], ex : Expr, rt : Option[Type]) extends Expr // function name(x1,x2,...) { ex }
  case class  CallExpr(e1 : Expr, e2s : List[Expr]) extends Expr                           // e1(e2, ...)
  case class  ReturnExpr(e1 : Expr) extends Expr                                           // return e1

  case class  AssignExpr(x : Expr, e1 : Expr, e2 : Expr) extends Expr // v = e1; e2
  case class  FieldExpr(e1 : Expr, field : String) extends Expr       // x.f
  case class  ObjectExpr(fields : List[(String,Expr)]) extends Expr   // { f1:e1, ... }

  // errors
  case class DynamicTypeError(e : Expr) extends Exception
  case class StaticTypeError(e : Expr) extends Exception
  case class UnimplementedError(e : Expr) extends Exception

  // types
  sealed trait Type
  case object UnitType extends Type
  case object BoolType extends Type
  case object NumType extends Type
  case object StringType extends Type
  case class  TupleType(params : List[Type]) extends Type
  case class  FunctionType(param : Type, ret : Type) extends Type

  // checks if ty1 can be automatically converted (cast) to ty2
  def canConvert(ty1 : Type, ty2 : Type) : Boolean = {
    (ty1,ty2) match {
      case (FunctionType(pt1,rt1),FunctionType(pt2,rt2)) =>
        canConvert(pt1,pt2) && canConvert(rt1,rt2)
      case (TupleType(ts1),TupleType(ts2)) => {
        if(ts1.length == ts2.length) {
          val ts = ts1 zip ts2
          ts.foldLeft(true){ case (res,(t1,t2)) =>
            res && canConvert(t1,t2)
                          }
        } else false
      }
      case (_,FunctionType(_,_)) => false
      case (_,TupleType(_)) => false
      case (FunctionType(_,_),_) => true
      case (_,StringType) => true
      case (_,NumType) => true
      case (_,BoolType) => true
      case (_,UnitType) => true
    }
  }

  def strValue(v : Value) : String = {
    v match {
      case NumVal(f) => ""+f
      case BoolVal(b) => ""+b
      case StringVal(s) => "\""+s+"\""
      case UndefVal => "undefined"
      case ClosureVal(m, l) => strExpr(l)
      case _ => ""
    }
  }

  def strExpr(e : Expr) : String = {
    e match {
      case ValueExpr(v) => strValue(v)
      case BopExpr(e1, op, e2) => "("+strExpr(e1)+" "+ppBop(op)+" "+strExpr(e2)+")"
      case UopExpr(op, e1) => ppUop(op)+"("+strExpr(e1)+")"
      case VarExpr(x) => x
      case IfExpr(c, e1, e2) => "("+strExpr(c)+" ? "+strExpr(e1)+" : "+strExpr(e2)+")"
      case LetExpr(t, x, e1, e2) => (t match { case Immutable => "const" case Mutable => "let"})+" "+x+" = "+strExpr(e1)+"; "+strExpr(e2)
      case PrintExpr(e1) => "console.log("+strExpr(e1)+")"
      case LambdaExpr(name,xs,ex,_) => "function "+(name match { case Some(name) => name case None => ""})+"("+xs.map({case(x,_)=>x}).mkString(", ")+") { "+strExpr(ensureRetExpr(ex))+" }"
      case CallExpr(VarExpr(s:String), e2s) => s+"("+e2s.map(strExpr).mkString(", ")+")"
      case CallExpr(e1, e2s) => "("+strExpr(e1)+")("+e2s.map(strExpr).mkString(", ")+")"
      case ReturnExpr(e1) => "return "+strExpr(e1)
      case AssignExpr(x, e1, e2) => strExpr(x)+" = "+strExpr(e1)+"; "+strExpr(e2)
      case FieldExpr(e1, field) => strExpr(e1)+"."+field
      case ObjectExpr(fields) => "{ "+fields.map({ case (key,v)=>key+":"+strExpr(v) }).mkString(", ")+" }"
    }
  }

  def ppBop(op : Bop) : String = {
    op match {
      case AndBop => "&&"
      case OrBop => "||"
      case PlusBop => "+"
      case MinusBop => "-"
      case TimesBop => "*"
      case DivBop => "/"
      case EqBop => "==="
      case NeqBop => "!=="
      case LtBop => "<"
      case LteBop => "<="
      case GtBop => ">"
      case GteBop => ">="
    }
  }

  def ppUop(op : Uop) : String = {
    op match {
      case NotUop => "!"
      case NegUop => "-"
    }
  }

  def v(a : Any) : Value = {
    a match {
      case (f : Float) => NumVal(f)
      case (i : Int) => NumVal(i.toFloat)
      case (d : Double) => NumVal(d.toFloat)
      case (b : Boolean) => BoolVal(b)
      case (s : String) => StringVal(s)
      case (u : Value) => u
      case _ => UndefVal
    }
  }

  def ve(a : Any) : Expr = {
    ValueExpr(v(a))
  }

  def isFloat(s : String) : Boolean = {
    !s.isEmpty && s.forall(x => x.isDigit || x=='.')
  }

  def toNum(v : Value) : Float = {
    v match {
      case NumVal(f) => f
      case BoolVal(true) => 1f
      case StringVal(s) => if(isFloat(s)) s.toFloat else Float.NaN
      case BoolVal(false) => 0f
      case UndefVal => Float.NaN
      case ClosureVal(_, _) => Float.NaN
      case ReferenceVal(_) => Float.NaN
    }
  }

  def toBool(v : Value) : Boolean = {
    v match {
      case NumVal(f) => if(f==0f || f==Float.NaN) false else true
      case BoolVal(b) => b
      case StringVal("") => false
      case StringVal(_) => true
      case UndefVal => false
      case ClosureVal(_, _) => true
      case ReferenceVal(_) => true
    }
  }

  def toStr(v : Value) : String = {
    v match {
      case NumVal(f) => ""+f
      case BoolVal(b) => ""+b
      case StringVal(s) => s
      case UndefVal => "undefined"
      case ClosureVal(_, l) => strExpr(l)
      case ReferenceVal(_) => "[object Object]"
    }
  }

  def lookupField(o : HeapObject, f : String) : Value = {
    o.find((x) => {
      val (key,_) = x
      key==f
    }) match {
      case Some((_,v)) => v
      case None => throw DynamicTypeError(FieldExpr(ObjectExpr(o.map({ case (key,v) => (key,ValueExpr(v))})),f))
    }
  }

  def updateField(o : HeapObject, f : String, v2 : Value) : HeapObject = {
    o.map((x) => {
      val (key,v) = x
      if(key==f) (key,v2) else (key,v)
    })
  }

  // obtain most recent binding of variable x in the environment
  def readEnvironment(env : Environment, x : String) : (VarAccess,Value) = {
    val (a,s) = env
    if(!a(0).contains(x)) println(a)
    s(a(0)(x))
  }

  // push a new binding of x into the environment
  def pushEnvironment(env : Environment, x : String, ae : (VarAccess,Value)) : Environment = {
    val id = uniqueId
    uniqueId = uniqueId + 1
    val (a,s) = env
    val a2 = a match {
      case m::more => (m + (x -> id))::more
      case _ => List(Map(x -> id))
    }
    (a2,s + (id -> ae))
  }

  // overwrite the most recent binding of x in the environment
  def overwriteEnvironment(env : Environment, x : String, ae : (VarAccess,Value)) : Environment = {
    val (a,s) = env
    a match {
      case m::more => (a, s + (m(x) -> ae))
        case Nil => throw new java.util.NoSuchElementException("var not found: "+x)
    }
  }

  def replValue(v : Value, f : Expr => Expr) : Value = {
    v match {
      case ClosureVal(m, LambdaExpr(name,xs,ex,rt)) => ClosureVal(m, LambdaExpr(name,xs,replExpr(ex,f),rt))
      case _ => v
    }
  }

  def replExpr(ei : Expr, f : Expr => Expr) : Expr = {
    val e = f(ei)
    e match {
      case ValueExpr(v) => ValueExpr(replValue(v,f))
      case BopExpr(e1, op, e2) => BopExpr(replExpr(e1,f), op, replExpr(e2,f))
      case UopExpr(op, e1) => UopExpr(op, replExpr(e1,f))
      case VarExpr(x) => e
      case IfExpr(c, e1, e2) => IfExpr(replExpr(c,f), replExpr(e1,f), replExpr(e2,f))
      case LetExpr(t, x, e1, e2) => LetExpr(t, x, replExpr(e1,f), replExpr(e2,f))
      case PrintExpr(e1) => PrintExpr(replExpr(e1,f))
      case LambdaExpr(name,xs,ex,rt) => LambdaExpr(name,xs,replExpr(ex,f),rt)
      case CallExpr(e1, e2s) => CallExpr(replExpr(e1,f),e2s.map(e2 => replExpr(e2,f)))
      case ReturnExpr(e1) => ReturnExpr(replExpr(e1,f))
      case AssignExpr(x, e1, e2) => AssignExpr(x, replExpr(e1,f), replExpr(e2,f))
      case FieldExpr(e1, field) => FieldExpr(replExpr(e1,f),field)
      case ObjectExpr(fields) => ObjectExpr(fields.map({case (k,v) => (k,replExpr(v,f))}))
    }
  }

  def ensureRetExpr(e : Expr) : Expr = {
    e match {
      case LetExpr(t, x, e1, e2) => LetExpr(t, x, e1, ensureRetExpr(e2))
      case AssignExpr(x, e1, e2) => AssignExpr(x, e1, ensureRetExpr(e2))
      case ReturnExpr(e) => e
      case _ => ReturnExpr(e)
    }
  }

  def parse(s : String) : Expr = SimpleJavaScriptParser.parse(s)

  object SimpleJavaScriptParser extends JavaTokenParsers {
    case class TypedVarList(l : List[TypedVar])
    case class ExprList(l : List[Expr])
    case class FieldList(l : List[(String,Expr)])
    case class TypeList(l : List[Type])
    case class TypeAnnot(t : Option[Type])

    def eof: Parser[String] = "\\z".r

    def prog: Parser[Expr] = (expr ~ eof) ^^ {
      case((e:Expr) ~ _) => e
    }

    def expr: Parser[Expr] = (stmt ~ rep(";" ~ stmt)) ^^ {
      case((s:Expr) ~ (sl)) => {
        val l = (s::(sl.map({case(_ ~ s)=>s}))).reverse
        val (start,more) = l.head match {
          case LetExpr(_,_,_,_) => (ValueExpr(UndefVal),l)
            case AssignExpr(_,_,_) => (ValueExpr(UndefVal),l)
              case ReturnExpr(e) => (ReturnExpr(e),l.tail)
                case e => (e,l.tail)
        }
        more.foldLeft(start){ case (result,s) =>
          s match {
            case LetExpr(t, x, e1, e2) => LetExpr(t, x, e1, result)
            case AssignExpr(x, e1, e2) => AssignExpr(x, e1, result)
            case ReturnExpr(e) => ReturnExpr(e)
            case e => {
              val id = uniqueId
              uniqueId = uniqueId + 1
              LetExpr(Immutable, "__temp"+id, e, result)
            }
          }
                           }
      }
    }
    def stmt: Parser[Expr] = (("return" ~ term) | (("const" | "let") ~ ident ~ "=" ~ term) | (term ~ "=" ~ term) | term) ^^ {
      case("return" ~ (e:Expr)) => ReturnExpr(e)
      case((t:String) ~ (x : String) ~ _ ~ (e1:Expr)) => LetExpr((t match { case "const" => Immutable case "let" => Mutable}), x, e1, ValueExpr(UndefVal))
      case((x : Expr) ~ _ ~ (e1:Expr)) => AssignExpr(x, e1, ValueExpr(UndefVal))
      case(s:Expr) => s
    }
    def term: Parser[Expr] = (term1 ~ rep("?" ~ term1 ~ ":" ~ term1)) ^^ {
      case((s:Expr) ~ (sl2)) => {
        val sl : List[(Expr,Expr)] = sl2.map({case(_ ~ (e1:Expr) ~ _ ~ (e2:Expr))=>(e1,e2)})
        val (sl3,out) = sl.foldLeft((Nil:List[(Expr,Expr)],s)){ case ((res,in),(e1,e2)) =>
          (res :+ (in,e1), e2)
                                                             }
        sl3.foldRight(out){ case((c,i),in) =>
          IfExpr(c,i,in)
                         }
      }
    }
    def term1: Parser[Expr] = (term2 ~ rep("||" ~ term2)) ^^ {
      case((s:Expr) ~ (sl)) => {
        sl.foldLeft(s){ case(res,_ ~ t) =>
          BopExpr(res, OrBop, t)
                     }
      }
    }
    def term2: Parser[Expr] = (term3 ~ rep("&&" ~ term3)) ^^ {
      case((s:Expr) ~ (sl)) => {
        sl.foldLeft(s){ case(res,_ ~ t) =>
          BopExpr(res, AndBop, t)
                     }
      }
    }
    def term3: Parser[Expr] = (term4 ~ rep(("===" | "!==") ~ term4)) ^^ {
      case((s:Expr) ~ (sl)) => {
        sl.foldLeft(s){ case(res,s ~ t) =>
          BopExpr(res, (s match { case "===" => EqBop case "!==" => NeqBop}), t)
                     }
      }
    }
    def term4: Parser[Expr] = (term5 ~ rep((">=" | ">" | "<=" | "<") ~ term5)) ^^ {
      case((s:Expr) ~ (sl)) => {
        sl.foldLeft(s){ case(res,s ~ t) =>
          BopExpr(res, (s match { case ">=" => GteBop case ">" => GtBop case "<=" => LteBop case "<" => LtBop}), t)
                     }
      }
    }
    def term5: Parser[Expr] = (term6 ~ rep(("+" | "-") ~ term6)) ^^ {
      case((s:Expr) ~ (sl)) => {
        sl.foldLeft(s){ case(res,s ~ t) =>
          BopExpr(res, (s match { case "+" => PlusBop case "-" => MinusBop}), t)
                     }
      }
    }
    def term6: Parser[Expr] = (term7 ~ rep(("*" | "/") ~ term7)) ^^ {
      case((s:Expr) ~ (sl)) => {
        sl.foldLeft(s){ case(res,s ~ t) =>
          BopExpr(res, (s match { case "*" => TimesBop case "/" => DivBop}), t)
                     }
      }
    }
    def term7: Parser[Expr] = (atom ~ rep("." ~ ident)) ^^ {
      case((s:Expr) ~ (sl)) => {
        sl.foldLeft(s){ case(res,s ~ t) =>
          FieldExpr(res, t)
                     }
      }
    }
    def field: Parser[(String,Expr)] = (ident ~ ":" ~ term) ^^ {
      case ((k:String) ~ _ ~ (v : Expr)) => (k,v)
    }
    def fieldList: Parser[FieldList] = (field ~ rep("," ~ field)) ^^ {
      case (s ~ sl) => FieldList(s::(sl.map({ case (_ ~ s) => s})))
    }
    def typ: Parser[Type] = (ta ~ rep("=>" ~ ta)) ^^ {
      case((s:Type) ~ (sl)) => {
        sl.foldLeft(s){ case(res,s ~ t) =>
          FunctionType(res, t)
                     }
      }
    }
    def ta: Parser[Type] = ("bool" | "num" | "str" | "unit" | "(" ~ typeList ~ ")") ^^ {
      case ("bool") => BoolType
      case ("num") => NumType
      case ("str") => StringType
      case ("unit") => UnitType
      case ("(" ~ TypeList(tl) ~ ")") => {
        tl match {
          case List(t) => t
          case _ => TupleType(tl)
        }
      }
    }
    def typeList: Parser[TypeList] = (typ ~ rep("," ~ typ)) ^^ {
      case((s:Type) ~ (sl)) => {
        TypeList(s::(sl.map({ case(s ~ t) => t })))
      }
    }
    def typeAnnot: Parser[TypeAnnot] = (":" ~ typ | "") ^^ {
      case ("") => TypeAnnot(None)
      case (":" ~ (t:Type)) => TypeAnnot(Some(t))
    }
    def typedIdent: Parser[TypedVar] = (ident ~ typeAnnot) ^^ {
      case ((s:String) ~ TypeAnnot(t)) => (s,t)
    }
    def typedLitList: Parser[TypedVarList] = (typedIdent ~ rep("," ~ typedIdent)) ^^ {
      case (s ~ sl) => TypedVarList(s::(sl.map({ case (_ ~ s) => s})))
    }
    def termList: Parser[ExprList] = (term ~ rep("," ~ term)) ^^ {
      case (s ~ sl) => ExprList(s::(sl.map({ case (_ ~ s) => s})))
    }
    def funcName: Parser[Expr] = (ident | "(" ~ term ~ ")") ^^ {
      case (x:String) => VarExpr(x)
      case ("(" ~ (e:Expr) ~ ")") => e
    }
    def atom: Parser[Expr] = (("console" ~ "." ~ "log" ~ "(" ~ term ~ ")") | ("function" ~ (ident | "") ~ "(" ~ (typedLitList | "") ~ ")" ~ typeAnnot ~ "{" ~ expr ~ "}") | "true" | "false" | "undefined" | floatingPointNumber | stringLiteral | (("!" | "-") ~ term) | (funcName ~ "(" ~ (termList | "") ~ ")") | "{" ~ (fieldList | "") ~ "}" | ident | ("(" ~ term ~ ")")) ^^ {
      case("console" ~ "." ~ "log" ~ "(" ~ (e:Expr) ~ ")") => PrintExpr(e)
      case("true") => ve(true)
      case("false") => ve(false)
      case("undefined") => ValueExpr(UndefVal)
      case(s:String) => if(Try(s.toFloat).isSuccess) ve(s.toFloat) else if(s.charAt(0) == '"') ve(s.substring(1,s.length()-1)) else VarExpr(s)
      case("function" ~ (name:String) ~ _ ~ (TypedVarList(l)) ~ _ ~ TypeAnnot(rt) ~ _ ~ (ex:Expr) ~ _) => LambdaExpr((name match { case "" => None case _ => Some(name)}), l, ex, rt)
      case("function" ~ (name:String) ~ _ ~ "" ~ _ ~ TypeAnnot(rt) ~ _ ~ (ex:Expr) ~ _) => LambdaExpr((name match { case "" => None case _ => Some(name)}), Nil, ex, rt)
      case("(" ~ (e:Expr) ~ ")") => e
      case("!" ~ (e:Expr)) => UopExpr(NotUop, e)
      case("-" ~ (e:Expr)) => UopExpr(NegUop, e)
      case((e:Expr) ~ "(" ~ ExprList(sl) ~ ")") => CallExpr(e, sl)
      case("{" ~ "" ~ "}") => ObjectExpr(Nil)
      case("{" ~ FieldList(sl) ~ "}") => ObjectExpr(sl)

    }

    def parse(s:String) : Expr = {
      val e = prog(new CharSequenceReader(s)).get
      // strip out any remaining return statements (we don't use return in our JS subset)
      replExpr(e, (x:Expr)=>{ x match { case ReturnExpr(e) => e case _ => x} })
    }
  }
}

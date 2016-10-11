// Version 1.8

import scala.collection.immutable.Set

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object CW1 {
  type Variable = String
  type Env[A] = Map[Variable,A]

  // Arithmetic expressions

  abstract class Expr
  case class Num(n: Integer) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Minus(e1: Expr, e2: Expr) extends Expr
  case class Times(e1: Expr, e2: Expr) extends Expr

  // Booleans
  case class Bool(n: Boolean) extends Expr
  case class Eq(e1: Expr, e2:Expr) extends Expr
  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr

  // Strings
  case class Str(s: String) extends Expr
  case class Length(e: Expr) extends Expr
  case class Index(e1: Expr, e2: Expr) extends Expr
  case class Concat(e1: Expr, e2: Expr) extends Expr

  // Variables and let-binding
  case class Var(x: Variable) extends Expr
  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr
  case class LetFun(f: Variable, arg: Variable, ty: Type, e1:Expr, e2:Expr)
  extends Expr
  case class LetRec(f: Variable, arg: Variable, xty: Type, ty: Type, e1:Expr, e2:Expr)
  extends Expr
  case class LetPair(x: Variable,y: Variable, e1:Expr, e2:Expr) extends Expr

  // Pairing
  case class Pair(e1: Expr, e2: Expr) extends Expr
  case class First(e: Expr) extends Expr
  case class Second(e: Expr) extends Expr

  // Functions
  case class Lambda(x: Variable, ty: Type, e: Expr) extends Expr
  case class Apply(e1: Expr, e2: Expr) extends Expr
  case class Rec(f: Variable, x: Variable, tyx:Type, ty: Type, e: Expr) extends Expr

  // Values
  abstract class Value
  case class NumV(n: Integer) extends Value
  case class BoolV(n: Boolean) extends Value
  case class StringV(s: String) extends Value
  case class PairV(v1: Value, v2: Value) extends Value
  case class ClosureV(env: Env[Value], x: Variable, e: Expr) extends Value
  case class RecV(env: Env[Value], f:Variable, x: Variable, e: Expr) extends Value

  // Types
  abstract class Type
  case object IntTy extends Type
  case object BoolTy extends Type
  case object StringTy extends Type
  case class PairTy(ty1: Type, ty2: Type) extends Type
  case class FunTy(ty1: Type, ty2: Type) extends Type

  // ======================================================================
  // Part 1: Syntactic transformation
  // ======================================================================

  // ======================================================================
  // Exercise 1: Capture-avoiding substitution
  // ======================================================================

  // This object provides a method to generate a "fresh" variable name
  object Gensym {
    private var id = 0
    def gensym(s: Variable): Variable = {
      val fresh_s = s + "_" + id
      id = id + 1
      fresh_s
    }
  }


  def subst(e1:Expr, e2:Expr, x: Variable): Expr =
    e1 match {
      case Num(e) => Num(e)
      case Plus(t1,t2) => Plus(subst(t1,e2,x),subst(t2,e2,x))
      case Minus(t1,t2) => Minus(subst(t1,e2,x),subst(t2,e2,x))
      case Times(t1,t2) => Times(subst(t1,e2,x),subst(t2,e2,x))

      case Var(y) =>
        if (x == y) {
          e2
        } else {
          Var(y)
        }
      case Let(y,t1,t2) =>
        if (x == y) { // we can stop early since x is re-bound here
          Let(y,subst(t1,e2,x),t2)
        } else { // otherwise, we freshen y
          val z = Gensym.gensym(y);
          val fresh_t2 = subst(t2,Var(z),y);
          Let(z,subst(t1,e2,x),subst(fresh_t2,e2,x))
        }
      case LetFun(f,arg,ty,t1,t2) => 
      {
        val fresh_arg = Gensym.gensym(arg);
        val fresh_f = Gensym.gensym(f);
        val fresh_t1 = subst(t1,Var(fresh_arg),arg);
        val fresh_t2 = subst(t2,Var(fresh_f),f);
        LetFun(fresh_f, fresh_arg, ty, subst(fresh_t1,e2,x), subst(fresh_t2,e2,x)) 
      }
      case LetRec(f,arg,xty,ty,t1,t2) =>
      {
        val fresh_arg = Gensym.gensym(arg);
        val fresh_f = Gensym.gensym(f);

        //for rec we also have to freshen f in t1
        val almost_fresh_t1 = subst(t1,Var(fresh_arg),arg);
        val fresh_t1 = subst(almost_fresh_t1, Var(fresh_f),f);
        val fresh_t2 = subst(t2,Var(fresh_f),f);
        LetRec(fresh_f, fresh_arg, xty, ty, subst(fresh_t1,e2,x), subst(fresh_t2,e2,x))
      }
      case LetPair(y1,y2,t1,t2) =>
      {
        val fresh_y1 = Gensym.gensym(y1);
        val fresh_y2 = Gensym.gensym(y2);
        val almost_fresh_t2 = subst(t2,Var(fresh_y1),y1);
        val fresh_t2 = subst(almost_fresh_t2, Var(fresh_y2),y2);
        LetPair(fresh_y1,fresh_y2, subst(t1,e2,x), subst(fresh_t2, e2, x))
      }
      case Lambda(y,ty,e) =>
      {
        val fresh_y = Gensym.gensym(y);
        val fresh_e = subst(e, Var(fresh_y),y)
        Lambda(fresh_y, ty, subst(fresh_e,e2,x))
      }
      case Rec(f,y,tyy,ty,e) =>
      {
        val fresh_f = Gensym.gensym(f);
        val fresh_y = Gensym.gensym(y);
        val almost_fresh_e = subst(e, Var(fresh_f), f);
        val fresh_e = subst(almost_fresh_e, Var(fresh_y), y);
        Rec(fresh_f, fresh_y, tyy, ty, subst(fresh_e, e2, x))
      }
      case Bool(n) => 
        Bool(n)
      case Eq(t1,t2) =>
        Eq(subst(t1,e2,x), subst(t2,e2,x));
      case IfThenElse(t,t1,t2) =>
        IfThenElse(subst(t,e2,x),subst(t1,e2,x), subst(t2,e2,x))
      case Str(s) =>
        Str(s)
      case Length(t) => 
        Length(subst(t,e2,x))
      case Index(t1,t2) =>
        Index(subst(t1,e2,x), subst(t2,e2,x))
      case Concat(t1,t2) =>
        Concat(subst(t1,e2,x), subst(t2,e2,x))
      case Pair(t1,t2) =>
        Pair(subst(t1,e2,x), subst(t2,e2,x))
      case First(t) =>
        First(subst(t,e2,x))
      case Second(t) =>
        Second(subst(t,e2,x))
      case Apply(t1,t2) =>
        Apply(subst(t1,e2,x), subst(t2,e2,x))
      case _ => sys.error("subst: todo")
    }


  // ======================================================================
  // Exercise 2: Desugaring let fun, let rec and let pair
  // ======================================================================

  def desugar(e: Expr): Expr = e match {

    case Num(n) => Num(n)
    case Plus(e1,e2) => Plus(desugar(e1),desugar(e2))
    case Minus(e1,e2) => Minus(desugar(e1),desugar(e2))
    case Times(e1,e2) => Times(desugar(e1),desugar(e2))
    case Let(f,e1,e2) => Let(f, desugar(e1), desugar(e2))
    case Var(x) => Var(x) 
    case LetPair(x,y,e1,e2) => 
    {
      val p = Gensym.gensym("p")
      val e2_tmp = subst(e2, First(Var(p)), x)
      val e2_fin = subst(e2_tmp, Second(Var(p)), y)
      Let(p, desugar(e1), desugar(e2_fin))
    }
    case LetFun(f,arg,ty,e1,e2) =>
      Let(f, Lambda(arg,ty,desugar(e1)), desugar(e2))
    case LetRec(f,arg,xty,ty,e1,e2) =>
      Let(f,Rec(f,arg,xty,ty,desugar(e1)), desugar(e2))
    case Pair(e1,e2) => Pair(desugar(e1),desugar(e2))
    case First(e1) => First(desugar(e1))
    case Second(e1) => Second(desugar(e1))
    case Lambda(x,ty,e1) => Lambda(x,ty,desugar(e1))
    case Apply(e1,e2) => Apply(desugar(e1), desugar(e2))
    case Rec(f,x,tyx,ty,e1) => Rec(f,x,tyx,ty,desugar(e1))
    case Bool(n) => Bool(n)
    case Eq(e1,e2) => Eq(desugar(e1), desugar(e2))
    case IfThenElse(e0,e1,e2) => IfThenElse(desugar(e0), desugar(e1), desugar(e2))
    case Str(s) => Str(s)
    case Length(e) => Length(desugar(e))
    case Index(e1,e2) => Index(desugar(e1),desugar(e2))
    case Concat(e1,e2) => Concat(desugar(e1),desugar(e2))

    case _ => sys.error("desugar: todo")

  }


  // ======================================================================
  // Part 2: Interpretation
  // ======================================================================

  // ======================================================================
  // Exercise 3: Primitive operations
  // ======================================================================


  object Value {
    // utility methods for operating on values
    def add(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV (v1 + v2)
      case _ => sys.error("arguments to addition are non-numeric")
    }

    def subtract(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV (v1 - v2)
      case _ => sys.error("arguments to addition are non-numeric")
    }

    def multiply(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 * v2)
      case _ => sys.error("arguments to multiplication are non-numeric")
    }

    def eq(v1: Value, v2: Value): Value = (v1,v2) match {
      case(NumV(v1),NumV(v2)) => BoolV(v1==v2)
      case(StringV(v1),StringV(v2)) => BoolV(v1==v2)
      case(BoolV(v1),BoolV(v2)) => BoolV(v1==v2)
      case _ => sys.error("arguments to equals are not correct")
    }

    def length(v: Value): Value = v match {
      case StringV(v1) => NumV(v1.length())
      case _ => sys.error("argument to length is not string")
    }

    def index(v1: Value, v2: Value): Value = (v1,v2) match {
      case(StringV(v1), NumV(v2)) => 
      {
        StringV(v1(v2)+"")
      }
      case _ => sys.error("wrong arguments to index")
    }

    def concat(v1: Value, v2: Value): Value = (v1,v2) match {
      case(StringV(v1), StringV(v2)) => StringV(v1+v2)
      case _ => sys.error("wrong argiments to concat")
    }
  }

  // ======================================================================
  // Exercise 4: Evaluation
  // ======================================================================

  def eval (env: Env[Value], e: Expr): Value = e match {
    // Arithmetic
    case Num(n) => NumV(n)
    case Plus(e1,e2) => 
      Value.add(eval(env,e1),eval(env,e2))
    case Minus(e1,e2) => 
      Value.subtract(eval(env,e1),eval(env,e2))
    case Times(e1,e2) =>
      Value.multiply(eval(env,e1),eval(env,e2))
    case Bool(n) => BoolV(n)
    case Eq(e1,e2) => Value.eq(eval(env,e1), eval(env,e2))
    case IfThenElse(e0,e1,e2) => eval(env,e0) match {
      case BoolV(true) => eval(env,e1)
      case BoolV(false) => eval(env,e2)
      case _ => sys.error("eval IfThenElse e0 val unametched")
    }
    case Str(s) => StringV(s)
    case Length(e0) => Value.length(eval(env,e0))
    case Index(e1,e2) => Value.index(eval(env,e1),eval(env,e2))
    case Concat(e1,e2) => Value.concat(eval(env,e1),eval(env,e2))
    case Var(x) => env(x)
    case Let(x,e1,e2) => eval(env + (x -> eval(env,e1)), e2)
    case Pair(e1,e2) => PairV(eval(env,e1),eval(env,e2))
    case First(e1) => eval(env,e1) match {
      case PairV(v1,v2) => v1 
      case _ => sys.error("eval First eval(env,e1) unamatched")
    }
    case Second(e1) => eval(env,e1) match {
      case PairV(v1,v2) => v2
      case _ => sys.error("eval Second eval(env,e1) unamtched")
    }
    case Lambda(x,ty,e1) => ClosureV(env, x, e1)
    case Rec(f,x,tyx,ty,e1) => RecV(env, f, x, e1)
    case Apply(e1,e2) => eval(env,e1) match 
    {
      case ClosureV(envC, xC, eC) => 
        eval(envC + (xC -> eval(env,e2)), eC)
      case RecV(envC, fC, xC, eC) => 
        eval(
          envC + (fC -> eval(env,e1)) + (xC -> eval(env,e2))
          , eC)
    }
    case _ => 
    {
      sys.error("eval: todo")
    }
  }


  // ======================================================================
  // Part 3: Typechecking
  // ======================================================================

  // ======================================================================
  // Exercise 5: Typechecker
  // ======================================================================
  def tyEq(e1: Type, e2:Type) : Boolean = (e1,e2) match
  {
    case (BoolTy,BoolTy) => true
    case (IntTy,IntTy) => true
    case (StringTy,StringTy) => true
    case (PairTy(t1,t2),PairTy(t3,t4)) => tyEq(t1,t3) && tyEq(t2,t4)
    case (FunTy(t1,t2), FunTy(t3,t4)) => tyEq(t1,t3) && tyEq(t2,t4) 
    case _ => false
  }

  // typing: calculate the return type of e, or throw an error
  def tyOf(ctx: Env[Type], e: Expr): Type = e match {
    // Arithmetic
    case Num(n) => IntTy
    case Plus(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => sys.error("non-integer arguments to +") 
    }
    case Minus(e1,e2) => (tyOf(ctx,e1), tyOf(ctx,e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => sys.error("non-integer arguments to -")
    }
    case Times(e1,e2) => (tyOf(ctx,e1), tyOf(ctx,e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => sys.error("non-integer arguments to *")
    }
    case Bool(n) => BoolTy
    case Eq(e1,e2) => (tyOf(ctx,e1), tyOf(ctx,e2)) match {
      case (IntTy, IntTy) => BoolTy
      case (BoolTy, BoolTy) => BoolTy
      case (StringTy, StringTy) => BoolTy
      case _ => 
      {
        sys.error("invalid arguments to ==")
      }
    }  
    
    case IfThenElse(e0,e1,e2) => (tyOf(ctx,e0), tyEq(tyOf(ctx,e1), tyOf(ctx,e2))) match { 
    
      case (BoolTy, true) => tyOf(ctx,e1)
      case _ => sys.error("invalid arguments to ifthenelse")
    }
    
    case Str(s) => StringTy
    case Length(e1) => (tyOf(ctx,e1)) match {
      case StringTy => IntTy
      case _ => sys.error("invalid types for length")
    }
    case Index(e1,e2) => (tyOf(ctx,e1), tyOf(ctx,e2)) match {
      case (StringTy, IntTy) => StringTy
      case _ => sys.error("invalid types for index")
    }
    case Concat(e1,e2) => (tyOf(ctx,e1), tyOf(ctx,e2)) match {
      case (StringTy, StringTy) => StringTy
      case _ => sys.error("invalid typef for concatination")
    }
    
    // Variables and let-binding
    case Var(x) => ctx(x)
    case Let(x,e1,e2) => tyOf(ctx + (x -> (tyOf(ctx,e1))), e2)
    case LetFun(f,arg,ty,e1,e2) => tyOf(ctx + (f -> FunTy(ty, tyOf(ctx + (arg -> ty), e1))), e2)
    case LetRec(f,arg,xty,ty,e1,e2) => tyOf(ctx + (f -> FunTy(xty, tyOf(
      ctx + (arg -> xty) + (f -> FunTy(xty,ty)), e1))), e2)
    case LetPair(x,y,e1,e2) => tyOf(ctx,e1)  match {
      case PairTy(t1,t2) => tyOf(ctx + (x->t1) + (y->t2),e2)
      case _ => sys.error("invalid type for pair (*)")
    }
    
    case Pair(e1,e2) => PairTy(tyOf(ctx,e1),tyOf(ctx,e2))
    case First(e1) => tyOf(ctx,e1)  match {
      case PairTy(t1,t2) => t1
      case _ => sys.error("invalid type for first")
    }
    case Second(e1) => tyOf(ctx,e1)  match {
      case PairTy(t1,t2) => t2
      case _ => 
      {
        println(ctx)
        sys.error("invalid type for second")
      }
    }
    case Lambda(x,ty,e1) => FunTy(ty, tyOf(ctx + (x -> ty),e1))
    case Apply(e1,e2) => (tyOf(ctx,e1), tyOf(ctx,e2)) match {
      case (FunTy(t1,t2), t3) => tyEq(t1,t3) match {
        case true => t2
        case false => sys.error("invalid types for apply (**)")
      }
      case (t1,t2)  => 
        sys.error("invalid types for apply (*)")
    }
    case Rec(f,x,tyx,ty,e1) => FunTy(tyx, tyOf(ctx + (x -> tyx) + (f -> ty), e1))
    

    case _ => sys.error("tyOf: todo")
  }


  // ======================================================================
  // Part 4: Some simple programs
  // ======================================================================

  // The following examples illustrate how to embed L_CW1 source code into
  // Scala using multi-line comments, and parse it using parser.parseStr.

  // Example 1: the swap function
  def example1: Expr = parser.parseStr("""
    let fun swap(x:int * int) = (snd(x), fst(x)) in 
    swap(42,17)
    """)

  // Example 2: the factorial function, yet again
  def example2: Expr = parser.parseStr("""
    let rec fact(n:int):int = 
      if (n == 0) then 1 else n * fact(n - 1) in 
      fact(5)
    """)

  // Example 3: exponentiation
  def example3: Expr = parser.parseStr("""
    let rec power(input: int * int):int =
      let (x,n) = input in
      if (n == 0) then 1 else
      x * power(x,n-1)
    in
    power(2,10)
    """)

  // Example 4: check whether two strings have the same last character
  def example4: Expr = parser.parseStr("""
    let fun sameLastChar(input: str * str) = 
      let (s1,s2) = input in 
      index(s1,length(s1)-1) == index(s2,length(s2)-1)
      in sameLastChar("abcz","abcdefghijklmnopqrstuvwxyz")
      """)


  // ======================================================================
  // Exercise 6: Fibonacci sequence
  // ======================================================================

  def fib: Expr = parser.parseStr("""
  rec fib(input: int): int.( 
    if(input == 0) then 0
    else ( 
     if(input == 1) then 1 else fib(input - 1) + fib(input - 2)
    )
  )"""
  )

  // ======================================================================
  // Exercise 7: Substrings
  // ======================================================================
//tests for scala console
//CW1.eval(Map.empty, CW1.Apply(CW1.desugar(CW1.substring), CW1.Pair(CW1.Str("ab"), CW1.Str("bcbcacacbb"))))
  def substring: Expr = parser.parseStr("""
  let rec check_at_pos(x : str * (str * (int * int))) : bool =
    let a = fst(x) in 
    let b = fst (snd(x)) in
    let start_pos = fst (snd (snd (x))) in
    let i = snd (snd (snd (x))) in 
    if(i == length(b)) then true
    else if index(a, start_pos + i) == index(b,i) then
    check_at_pos((a, (b, (start_pos, i+1)))) else false
  in
  let rec check_substring(x : str * (str * int)) : bool = 
    let a = fst(x) in
    let b = fst(snd(x)) in
    let i = snd(snd(x)) in
    if(i + length(b)  == length(a) + 1) then false
    else if check_at_pos(( a, (b, (i, 0)) )) then true
    else check_substring(( a, (b, i+1) ))
  in
  \x:(str * str).( 
    let a = fst(x) in
    let b = snd(x) in
    check_substring(b, (a, 0)))"""
  )

  /*======================================================================
    The rest of this file is support code, which you should not (and do not
      need to) change.
  ====================================================================== */

 class CWParser extends StandardTokenParsers with PackratParsers {

   type P[+A] = PackratParser[A]

   def parseStr(input: String): Expr = {
     phrase(expression)(new lexical.Scanner(input)) match {
       case Success(ast, _) => ast
       case e: NoSuccess => sys.error(e.msg)
     }
   }

   def parse(input: String): Expr = {
     val source = scala.io.Source.fromFile(input)
     val lines = try source.mkString finally source.close()
     parseStr(lines)
   }

   lexical.reserved += ("let", "in", "rec", "if", "then", "else",
     "int","str","bool","true","false","fst","snd","concat",
     "index","length","fun"
     )
   lexical.delimiters += ("=","*", "\\", "+", "-", "(", ")", "==", ":", ".",
     "->", ","
     )

   lazy val expression: P[Expr] =
     simpleExpr

     lazy val lambda: P[Expr] =
       ("\\" ~> ident) ~ (":" ~> typ) ~ ("." ~> expression) ^^ {
         case arg~ty~body => Lambda(arg,ty,body)
       }

       lazy val rec: P[Expr] =
         ("rec" ~> ident) ~
         ("(" ~> ident) ~ (":" ~> typ) ~ ((")" ~ ":") ~> typ) ~
         ("." ~> expression) ^^ {
           case recArg~funArg~funType~recType~body =>
             Rec(recArg,funArg,funType,recType,body)
         }

         lazy val ifExpr: P[Expr] =
           ("if" ~> expression) ~
           ("then" ~> expression) ~
           ("else" ~> expression) ^^ {
             case cond~e1~e2 => IfThenElse(cond,e1,e2)
           }

           lazy val letExpr: P[Expr] =
             ("let" ~> ident) ~ ("=" ~> expression) ~ ("in" ~> expression) ^^ {
               case binder~e1~e2 => Let(binder,e1,e2)
             }

             lazy val letFun: P[Expr] =
               ("let" ~ "fun" ~> ident) ~ ("(" ~> ident) ~
               (":" ~> typ <~ ")") ~ ("=" ~> expression) ~
               ("in" ~> expression) ^^ {
                 case fun~binder~ty~e1~e2 => LetFun(fun,binder,ty,e1,e2)
               }

               lazy val letRec: P[Expr] =
                 ("let" ~ "rec" ~> ident) ~ ("(" ~> ident) ~
                 (":" ~> typ <~ ")") ~ (":" ~> typ) ~ ("=" ~> expression) ~
                 ("in" ~> expression) ^^ {
                   case fun~binder~xty~ty~e1~e2 => LetRec(fun,binder,xty,ty,e1,e2)
                 }

                 lazy val letPair: P[Expr] =
                   ("let" ~ "(") ~> ident ~ ("," ~> ident <~ ")") ~
                   ("=" ~> expression) ~ ("in" ~> expression) ^^ {
                     case x~y~e1~e2 => LetPair(x,y,e1,e2)
                   }

                   lazy val typ: P[Type] =
                     funTyp 

                     lazy val funTyp: P[Type] =
                       pairTyp ~ "->" ~ funTyp ^^ {
                         case t1~"->"~t2 => FunTy(t1,t2)
                       } | pairTyp

                       lazy val pairTyp: P[Type] =
                         primitiveType ~ "*" ~ pairTyp ^^ {
                           case t1~"*"~t2 => PairTy(t1,t2)
                         } | primitiveType

                         lazy val primitiveType: P[Type] =
                           "bool" ^^^ BoolTy | "int" ^^^ IntTy | "str" ^^^ StringTy |  "("~>typ<~")"

                           lazy val operations: P[Expr] =
                             application | 
                             ("fst" ~ "(") ~> expression <~ ")" ^^ (x => First(x)) |
                             ("snd" ~ "(") ~> expression <~ ")" ^^ (x => Second(x)) |
                             ("length" ~ "(") ~> expression <~ ")" ^^ (x => Length(x)) |
                             ("concat"  ~ "(") ~> expression ~ ("," ~> expression) <~ ")" ^^ {
                               case e1~e2 => Concat(e1,e2)
                             } |
                             ("index" ~ "(") ~> expression ~ ("," ~> expression) <~ ")" ^^ {
                               case e1~e2 => Index(e1,e2)
                             }

                             lazy val arith: P[Expr] =
                               eq

                               lazy val prod: P[Expr] =
                                 prod ~ "*" ~ fact ^^ {
                                   case e1~"*"~e2 => Times(e1,e2)
                                 } | fact

                                 lazy val summation: P[Expr] =
                                   summation ~ "+" ~ prod ^^ {
                                     case e1~"+"~e2 => Plus(e1,e2)
                                     } | summation ~ "-" ~ prod ^^ {
                                       case e1~"-"~e2 => Minus(e1,e2)
                                     } | prod

                                     lazy val eq: P[Expr] =
                                       simpleExpr ~ "==" ~ summation ^^ {
                                         case e1~"=="~e2 => Eq(e1,e2)
                                       } | summation

                                       lazy val application: P[Expr] =
                                         fact ~ fact ^^ {
                                           case e1~e2 => Apply(e1,e2)
                                         }

                                         lazy val simpleExpr: P[Expr] = (
                                           lambda |
                                           rec |
                                           letExpr |
                                           letFun |
                                           letRec |
                                           letPair |
                                           ifExpr |
                                           arith |
                                           fact
                                         )

                                         lazy val pairLit: P[Expr] =
                                           "(" ~> expression ~ "," ~ expression <~ ")" ^^ {
                                             case t1~","~t2 => Pair(t1,t2)
                                           }

                                           lazy val fact: P[Expr] = (
                                             operations |
                                             pairLit |
                                             (ident ^^ Var) |
                                             (numericLit ^^ { x => Num(x.toInt) }) |
                                             (stringLit ^^ Str) |
                                             ("true" ^^^ Bool(true)) |
                                             ("false" ^^^ Bool(false)) |
                                             "("~>expression<~")"
                                           )

 }


 val parser = new CWParser


 object Main {
   def typecheck(ast: Expr):Type =
     tyOf(Map.empty,ast);

   def evaluate(ast: Expr):Value =
     eval(Map.empty,ast)



   def showResult(ast: Expr) {
     println("AST:  " + ast.toString + "\n")

     print("Type Checking...");
     val ty = typecheck(ast);
     println("Done!");
     println("Type of Expression: " + ty.toString + "\n") ;

     println("Desugaring...");
     val core_ast = desugar(ast);
     println("Done!");
     println("Desugared AST: " + core_ast.toString + "\n") ;

     println("Result: " + evaluate(core_ast));
   }

   def start(): Unit = {
     println("Welcome to Giraffe! (V1.8, October 16, 2015)");
     println("Enter expressions to evaluate, :load <filename.gir> to load a file, or :quit to quit.");
     repl()
   }

   def repl(): Unit = {
     print("Giraffe> ");
     val input = scala.io.StdIn.readLine();
     if(input == ":quit") {
       println("Goodbye!")
     }
     else if (input.startsWith(":load")) {
       val ast = parser.parse(input.substring(6));
       showResult(ast);
       repl()
     } else {
       try {
         val ast = parser.parseStr(input);
         showResult(ast)
       } catch {
         case e:Throwable => println("Error: " + e)
       }
       repl()
     }
   }

 }
 def main( args:Array[String] ):Unit = {
   if(args.length == 0) {
     Main.start()
   } else {
     try {
       print("Parsing...");
       val ast = parser.parse(args.head)
       println("Done!");
       Main.showResult(ast)
     } catch {
       case e:Throwable => println("Error: " + e)
     }
   }
 }
}




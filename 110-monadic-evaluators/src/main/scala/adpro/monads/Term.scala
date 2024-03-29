/* ADVANCED PROGRAMMING. Monadic Evaluators. Andrzej Wąsowski */
package adpro.monads
import scala.language.higherKinds

// AUTHORS:
// miev@itu.dk
// ecly@itu.dk

// Work through this file top down

// Section 2.1 [Wadler]

// Wadler uses a langauge similar to Haskell to implement his evaluator. We will
// use scala.  This is Wadler's Term language implemented in Scala:

trait Term
case class Cons (value :Int) extends Term
case class Div (left :Term, right :Term) extends Term


// From now on we create one module (object) per section to avoid name clashes
// between different variants.

// This is the basic evaluator (compare to the paper, to see whether you
// understand)

object BasicEvaluator {

  def eval (term :Term) :Int = term match {
    case Cons(a) => a
    case Div(t,u) => eval (t) / eval (u)
  }

}

// Uncomment sections below as you proceed

// Section 2.2 [Wadler] Variation one: Exceptions

object ExceptionEvaluator {

  // an implementation of Wadler's types in Scala
  type Exception = String
  trait M[+A]
  case class Raise (e: String) extends M[Nothing]
  case class Return[A] (a: A) extends M[A]

  // an implementation of direct exception evaluator in Scala:
  def eval (term :Term) :M[Int] = term match {
    case Cons(a) => Return (a)
    case Div(t,u) => eval(t) match {
      case Raise(e) => Raise(e)
      case Return(a) => eval(u) match {
        case Raise(e) => Raise(e)
        case Return(b) =>
          if (b == 0) {
            Raise("division by zero")
          } else {
            Return (a/b)
          }
      }
    }
  }

  // Once you are done reflect how massive was the change from the
  // BasicEvaluator to the exception evaluator (no need to write anything).
}

// Section 2.3 [Wadler] Variation two: State
object StateEvaluator {
  type State = Int
  case class M[+A] (step: State => (A,State))

  def eval (term :Term) :M[Int] = term match {
    case Cons (a) => M[Int] (x => (a,x))
    case Div (t,u) => M[Int] (s => {
      val (a, sa) = (eval (t)).step(s)
      val (b, sb) = (eval (u)).step(sa)
      (a/b, sb+1)
    })
  }

}

// // Section 2.4 [Wadler] Variation three: Output

object OutputEvaluator {

 type Output = String
 case class M[+A] (o: Output, a: A)

 def line (a :Term) (v :Int) :Output =
   "eval(" + a.toString + ") <= " + v.toString + "\n"

 def eval (term :Term) :M[Int] = term match {
    case Cons (a) => M[Int] (line(Cons(a))(a), a)
    case Div (t,u) => {
      val mx = eval (t)
      val my = eval (u)
      val res = mx.a/my.a
      M[Int] (mx.o + my.o + line(Div(t,u))(res), res)
    }
 }
}

// Section 2.5 [Wadler] A monadic evaluator

// The following are two generic monadic interfaces (one for classes, one for
// meta-classes/objects) that we will use to type check our monadic solutions.
//
// We shall provide flatMap and map for our monads to be able to use for
// comprehensions in Scala.
//
// IMPORTANT: flatMap is called "(*)" in the paper.

trait Monad[+A,M[_]] {
  def flatMap[B] (k: A => M[B]) :M[B]
  def map[B] (k: A => B) :M[B]
}

// we will provide unit, as the paper does. This will be placed in a companion
// object.

trait MonadOps[M[_]] { def unit [A] (a :A) :M[A] }

// The above abstract traits will be used to constraint types of all our monadic
// implementations, just to ensure better type safety and uniform interfaces.



// Now we are startin to implement the monadic evaluator from the paper.
// Compare this implementation to the paper, and make sure that you understand
// the Scala rendering.

// Section 2.6 [Wadler] Variation zero, revisited: The basic evaluator

object BasicEvaluatorWithMonads {
  // We enrich our M type with flatMap and map;
  // A flatMap is already in the paper (called *)
  // I add map, so that we can use for comprehensions with this type
  case class M[+A] (a: A) extends Monad[A,M] {
    def flatMap[B] (k: A => M[B]) :M[B] = k (this.a)
    def map[B] (k: A => B) :M[B] = M.unit (k (this.a))
  }

  // The paper also uses unit, so we put it in the companion object
  object M extends MonadOps[M] { def unit[A] (a : A) :M[A] = M[A] (a) }

  def eval (term: Term) :M[Int] = term match {
    case Cons (a) => M.unit (a)
    case Div (t,u) => for {
      a <- eval (t)
      b <- eval (u)
      r <- M.unit (a/b)
    } yield r
  }
  // TODO: Make sure that you understand the above implementation (an dhow it
  // relates to the one in the paper). If you find the for comprehension to be
  // obscuring things, you may want to rewrite the above using just map and
  // flatMap.
}

// Section 2.7 [Wadler] The monadic evaluator with exceptions
object ExceptionEvaluatorWithMonads {
  type Exception = String

  trait M[+A] extends Monad[A,M]{
    def flatMap[B] (k: A => M[B]) :M[B] = this match {
      case Raise (e) => Raise (e)
      case Return (a) => k(a)
    }

    def map[B] (k: A => B) :M[B] = this match {
      case Raise (e) => Raise (e)
      case Return (a) => Return (k(a))
    }
  }

  object M extends MonadOps[M] { def unit[A] (a : A) :M[A] = Return (a) }
  case class Raise (e: String) extends M[Nothing]
  case class Return[A] (a: A) extends M[A]

  def eval (term :Term) :M[Int] = term match {
    case Cons (a) => M.unit (a)
    case Div (t,u) => for {
      a <- eval(t)
      b <- eval(u)
      r <- if (b == 0) {
        Raise("division by zero")
      } else {
        M.unit(a/b)
      }
    } yield r
  }
}

// Section 2.8 [Wadler] Variation two, revisited: State

object StateEvaluatorWithMonads {
  type State = Int

  case class M[+A] (step: State => (A,State)) extends Monad[A,M] {

    // flatMap is bind or (*) in the paper
    def flatMap[B] (k :A => M[B]) = M[B] {
      x => { val (a,y) = this.step (x); k(a).step(y) }
    }

    def map[B] (k :A => B) :M[B] = M[B] {
      x => { val (a,y) = this.step(x); (k(a),y) }
    }
  }

  object M extends MonadOps[M] {
    def unit[A] (a : A) :M[A] = M[A] (s => (a, s))
    val tick :M[Unit] = M[Unit] (s => ((), s+1))
  }

  def eval (term :Term) :M[State] = term match {
    case Cons (a) => M.unit (a)
    case Div (t,u) => for {
      a <- eval(t)
      b <- eval(u)
      _ <- M.tick
    } yield a/b
  }
}

// // Section 2.9 [Wadler] Output evaluator
//
object OutputEvaluatorWithMonads {

 type Output = String

 case class M[+A] (o: Output, a: A) {

   // flatMap is (*) in [Wadler]
   def flatMap[B] (k :A => M[B]) = {
     val m = k (this.a)
     M[B] (this.o + m.o,m.a)
   }

   def map[B] (k :A => B) :M[B] = M[B] (this.o, k(this.a))
 }

 object M { def unit[A] (a : A) :M[A] = M[A]("", a) }

 def out (x: Output): M[Unit] = M[Unit](x,())

 def line (a :Term) (v :Int) :Output =
   "eval(" + a.toString + ") <= " + v.toString + "\n"

 def eval (term :Term) :M[Int] = term match {
    case Cons (a) => for (u <- out(line(term)(a))) yield a
    case Div (t,u) => for {
      a <- eval(t)
      b <- eval(u)
      u <- out(line(term)(a/b))
    } yield a/b
 }

 // Discuss in the group how the monadic evaluator with output differs from
 // the monadic basic one (or the one with state/counter).
}


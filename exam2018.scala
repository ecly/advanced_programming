// Name: Emil Christian Lyngeaard
// ITU email: ecly@itu.dk
package adpro.exam2018

import fpinscala.monoids.Monoid
import fpinscala.monads.Monad
import fpinscala.monads.Functor
import fpinscala.laziness.{Stream,Empty,Cons}
import fpinscala.laziness.Stream._
import fpinscala.parallelism.Par._
import scala.language.higherKinds
import adpro.data._
import adpro.data.FingerTree._
import monocle.Lens

object Q1 {

  def hasKey[K,V] (l: List[(K,V)]) (k: K) :Boolean =
      l exists { case (k1,v) => k1 == k }

  private def f[K,V] (acc: List[(K,List[V])], el: (K,V)) :List[(K,List[V])] =
    if (hasKey (acc) (el._1))
        acc.map { case (k,v) => if (k==el._1) (k,el._2::v) else (k,v) }
    else (el._1,List(el._2))::acc

  def groupByKey[K,V] (l :List[(K,V)]) :List[(K,List[V])] = {
    val acc : List[(K, List[V])] = Nil
    l.foldLeft (acc) (f _)
  }

  // Using a car:
  // def groupByKey[K,V] (l :List[(K,V)]) :List[(K,List[V])] =
  //   l.groupBy(_._1).map(x => (x._1, x._2.map(_._2))).toList
}


object Q2 {

  def f[A,B] (results: List[Either[A,B]]) :Either[List[A],List[B]] =  {
    if (results.exists(_.isLeft)) {
      Left(results.filter(_.isLeft).map{case Left(x) => x})
    } else  {
      Right(results.map{case Right(x) => x})
    }
  }
}


object Q3 {

  type T[B] = Either[String,B]
  implicit val eitherStringIsMonad :Monad[T] = new Monad[T] {
    def unit[A] (a: => A): T[A] = Right(a)
    override def flatMap[A,B] (ma: T[A]) (f: A => T[B]) :T[B] = ma.flatMap (f)
  }

  implicit def eitherIsMonad[A] = {
    type T[B] = Either[A,B]
    def unit[A] (a: => A): T[A] = Right(a)
    def flatMap[A,B] (ma: T[A]) (f: A => T[B]) :T[B] = ma.flatMap (f)
  }
}


object Q4 {

   // Write the answers in English below.

   // A. A stream with the Fibonacci numbers.
   // 1,1,2,3,5

   // B. All tuples with neighboring Fibonacci numbers,
   // starting with the tuple (1, 1), followed by (1, 2)...

}


object Q5 {

  def parForall[A] (as: List[A]) (p: A => Boolean): Par[Boolean] =  {
    val bs: Par[List[Boolean]] = parMap (as) (p)
    map[List[Boolean],Boolean] (bs) (bs => bs exists (!_) )
  }

}


object Q6 {

  def apply[F[_],A,B](fab: F[A => B])(fa: F[A]): F[B] = ???
  def unit[F[_],A](a: => A): F[A] = ???

  val f: (Int,Int) => Int = _ + _
  def a :List[Int] = ???

  // Answer below in a comment:
  // f.curried = (Int) => (Int) => (Int)
  //
  // x is of type List[Int]
  // We basically do two applys, such that f.curried first
  // is applied with its first parameter, then its second, resulting
  // in integers.

} // Q6


object Q7 {

  def map2[A,B,C] (a :List[A], b: List[B]) (f: (A,B) => C): List[C] = ???


  def map3[A,B,C,D] (a :List[A], b: List[B], c: List[C]) (f: (A,B,C) => D) :List[D] = {
    val temp : List[(A,B)] = map2(a,b){case (x,y) => (x,y)}
    map2(temp, c){case ((xa, xb),xc) => f(xa,xb,xc)}
  }

  def map3monad[A,B,C,D,M[_] <: Monad[M]] (a :M[A], b :M[B], c :M[C]) (f: (A,B,C) => D) :M[D] = {
    val temp : M[(A,B)] = a.map2(a,b){case (x,y) => (x,y)}
    a.map2(temp, c){case ((xa, xb),xc) => f(xa,xb,xc)}
  }
}


object Q8 {

  def filter[A] (t: FingerTree[A]) (p: A => Boolean): FingerTree[A] = {
    val empty = adpro.data.Empty ()
    reduceL[A,FingerTree[A]] { case (acc,a) => if (p(a)) acc addR a else acc} (empty, t)
  }

}


object Q9 {

  def eitherOption[A,B] (default: => A): Lens[Either[A,B],Option[B]] = {
    def tryGet (x : Either[A,B]) : Option[B] = x match {
      case Right(x) => Some(x)
      case _ => None
    }

    Lens[Either[A,B],Option[B]] (tryGet _) (x => _ => x match {
      case Some(y) => Right(y)
      case None => Left(default)
    })
  }
  // Answer the questions below:

  // A. Yes - when we eg. put Some(4), it will be Some(4) when we get,
  // despite being stored as Right(4)

  // B. No - if we have Left("blah") as x and our Lens as l, we get:
  // l.set(l.get(x))(x) == x, meaning
  // l.set(None)(Left(4)) == x which is false, as it will be Left(default)

  // C. Yes - since a second 'set' is independent of the first set's result,
  // which is indicated from the ignored second paramenter in our Lens defintion.

} // Q9


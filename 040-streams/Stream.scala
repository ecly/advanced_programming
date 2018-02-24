// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }

  // Exercise 2
  def toList: List[A] = (foldLeft (Nil: List[A]) (_::_)).reverse

  // Exercise 3
  def take (n: Int): Stream[A] = (this, n) match {
      case (Empty, _) => Empty
      case (_, 0) => Empty
      case (Cons (h, t), _) => Cons(h, () => t().take(n-1))
    }

  def drop (n: Int): Stream[A] = (this, n) match {
    case (Empty, _) => Empty
    case (_, 0) => this
    case (Cons(h, t), _) => t().drop(n-1)
  }

  // It is fast since only the first 51 values are considered, since the streams and take are lazy.

  // Exercise 4
  def takeWhile (p: A => Boolean): Stream[A] = this.headOption match {
    case None => Empty
    case Some(v) => if (p(v)) { Cons(() => v, () => this.tail.takeWhile(p)) } else { Empty }
  }

  // Similar to above, takeWhile is lazy, so only the first 150 values are considered.

  // Exercise 5
  def forAll (p: A => Boolean): Boolean = !this.exists((v) => !p(v))

  // Forall needs to look at every element, so if every value in an infinite stream has the property,
  // an infinite number values must be loked at.

  // Exercise 6
  def takeWhile2 (p: A => Boolean): Stream[A] =
    this.foldRight (Empty: Stream[A]) ((v, acc) => if (p(v)) {
      Cons(() => v, () => acc)
    } else {
      Empty
    })

  // Exercise 7
  def headOption2 () :Option[A] = this.foldRight (None: Option[A]) ((v, _) => Some(v))

  //def find (p :A => Boolean) :Option[A] = this.filter (p).headOption
}




case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq

  // Exercise 1
  def to (n: Int): Stream[Int] = Cons(() => n, () => to(n-1))

  def from (n: Int): Stream[Int] = Cons(() => n, () => from(n+1))

  val naturals = from(1)
}

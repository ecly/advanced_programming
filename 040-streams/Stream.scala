// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala
// AUTHOR1: miev@itu.dk
// AUTHOR2: ecly@itu.dk

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
  //def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match

  // Exercise 8
  def map[B] (f : A => B) : Stream[B] =
    this.foldRight (Empty : Stream[B]) ((a,b) => cons(f(a),b))

  def filter[B] (p : A => Boolean) : Stream[A] =
    this.foldRight (Empty : Stream[A]) ((a,b) => if (p(a)) cons(a, b) else b)

  def append[B>:A] (that : => Stream[B]) :Stream[B] = foldRight (that) (cons (_,_))

  def flatMap[B] (f : A => Stream[B]) : Stream[B] =
    this.foldRight (Empty : Stream[B]) ((a,b) => f(a).append(b))

  // For streams we will only filter until we're able to take the head,
  // whereas for lists we would filter the entire lists and then take the head.
  def find (p :A => Boolean) :Option[A]= this.filter(p).headOption

  // Exercise 13
  def map1[B] (f : A => B) : Stream[B] = unfold(this) {
      case Empty => None
      case Cons(h,t) => Some(f(h()), t())
  }

  def take1 (n : Int) : Stream[A] = unfold(this,n){
    case (Empty, _) => None
    case (Cons(_,_),0) => None
    case (Cons(h,t),n) => Some(h(), (t(),n-1))
  }

  def takeWhile1 (p : A => Boolean) : Stream[A] = unfold(this){
    case Empty => None
    case Cons(h,t) => if (p(h())) Some(h(), t()) else None
  }

  def zipWith1[B,C] (f : (=>A,=>B) => C) (that : Stream[B]) : Stream[C] = unfold(this, that){
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h,t), Cons(h1,t1)) => Some(f(h(),h1()), (t(),t1()))
  }
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

  // Exercise 10
  def fibs : Stream[Int] = {
    def f(x:Int)(y:Int) : Stream[Int] = cons(x, f(y)(y+x))
    f (0)(1)
  }

  // Exercise 11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((x,xs)) => cons(x, unfold(xs)(f))
  }

  // Exercise 12
  def from1 (n: Int): Stream[Int] = unfold (1)(n => Some(n,n+1))
  def fibs1 : Stream[Int] = unfold (0,1){case (x,y) => Some(x,(y,y+x))}
}

// Advanced Programming 2018,
// A. Wąsowski, Z.Fu. IT University of Copenhagen
//
// AUTHOR1: Emil Lynegaard ecly@itu.dk
// AUTHOR2: Michael Vesterli miev@itu.dk
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.
//
// Please only hand in files that compile (comment out or use '???' for the
// parts you don't know how to solve or cannot complete).

// An ADT of Lists

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2

  def tail[A] (as: List[A]) :List[A] = as match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }
  // Some possibilities would be to return nil or throw an exception.

  // Exercise 3

  def drop[A] (l: List[A], n: Int) : List[A] = (l, n) match {
    case (l, 0) => l
    case (Cons(_, tail), n) => drop(tail, n-1)
    case (Nil, _) => Nil
  }

  // Exercise 4

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(v, tail) => if (f(v)) { dropWhile(tail, f) } else { Cons(v, tail) }
    case Nil => Nil
  }

  // Exercise 5

  def init[A](l: List[A]): List[A] = l match {
    case Cons(v, Nil) => Nil
    case Cons(v, tail) => Cons(v, init(tail))
    case Nil => Nil
  }
  // It is linear time as the list must be reconstructed. It is therefore also linear space.

  // Exercise 6

  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }

  def length[A] (as: List[A]): Int = foldRight (as, 0) ((_, acc) => acc+1)

  // Exercise 7

  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B) : B = as match {
    case Nil => z
    case Cons(h, tail) => foldLeft (tail, f(z, h)) (f)
  }

  // Exercise 8

  def product (as :List[Int]) : Int = foldLeft (as, 1) (_*_)
  def length1 (as :List[Int]) : Int = foldLeft (as, 0) ((acc, _) => 1+acc)

  // Exercise 9

  def reverse[A] (as :List[A]) :List[A] = foldLeft (as, (Nil : List[A])) ((acc, v) => Cons(v, acc))

  // Exercise 10

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B =
    foldLeft (reverse(as), z)  ((x, y) => f (y, x))


  // Exercise 11

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]) :List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => append(h, concat(t))
  }

  // Exercise 12

  def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => if (f h) { filter(t) } else { Cons(h, filter(t)) }
  }

  // Exercise 13
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap (t) (f))
  }

  // Exercise 14
  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] =
    flatMap (l) (i => if(p(i)) Cons(i, Nil) else Nil)

  // Exercise 15
  def add (l: List[Int]) (r: List[Int]): List[Int] = (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h, t), Cons(h1, t1)) => Cons(h+h1, add(t)(t1))
  }

  // Exercise 16
  def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h, t), Cons(h1, t1)) => Cons(f(h,h1), zipWith(f)(t,t1))
  }

  // Exercise 17
  def hasSubsequence[A] (sup: List[A], sub: List[A]) :Boolean = {
    def startsWith[A] (x: List[A], y: List[A]) :Boolean = (x, y) match {
      case (_, Nil) => true
      case (Cons(h, t), Cons(h1, t1)) if h == h1 => startsWith(t, t1)
      case _ => false
    }
    (sup, sub) match{
      case (_, Nil) => true
      case (Cons(h, t), Cons(h1, t1)) =>
        if (h == h1 && startsWith(t, t1)) true
        else hasSubsequence(t, sub)
      case _ => false
    }
  }

  // Exercise 18
  def pascal (n :Int) : List[Int] = {
    def f (l: List[Int])(m: Int): List[Int] = {
      if (m == 1) l
      else {
        val l_padded = Cons(0, l)
        val r_padded = append(l, Cons(0, Nil))
        val row = zipWith ((x:Int,y:Int) => x+y) (l_padded, r_padded)
        f (row) (m-1)
      }
    }
    f(Cons(1,Nil))(n)
  }
}

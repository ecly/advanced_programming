// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1: ecly@itu.dk
// AUTHOR2: miev@itu.dk
//
// Write ITU email addresses of both group members that contributed to
// the solution of the exercise (in lexicographic order).
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

// The extension of App allows writing statements at class top level (the so
// called default constructor). For App objects they will be executed as if they
// were placed in the main method in Java.

object Exercises extends App {

  // Exercise 3

  def fib (n: Int) : Int = {
    @annotation.tailrec
    def f (n: Int, a: Int, b: Int) : Int = n match {
      case 1 => a
      case n => f(n-1, b, a+b)
    }
    f(n, 0, 1)
  }
  // some tests (uncomment, add more):

  assert (fib (1) == 0)
  assert (fib (2) == 1)
  assert (fib (3) == 1)
  assert (fib (4) == 2)
  assert (fib (5) == 3)
  assert (fib (6) == 5)

  // Exercise 4

  // A simple object describing a cost line; implemented imperatively, Java
  // style (this way until we learn more Scala)
  class Expense {

    // A constructor definition
    def this (tag :String, price :Int) = {
      this()
      this.tag = tag
      this.price = price
    }

    var tag   :String = "" // a tag line in the accounting system
    var price :Int    = 0 // the price is in cents
  }

  // computes the total of expenses in cents

  def total (expenses: Array[Expense]) : Int = {
    @annotation.tailrec
    def f (seq: List[Expense], acc: Int) : Int = seq match {
      case v::tail => f(tail, v.price+acc)
      case Nil => acc
    }
    f(expenses.toList, 0)
  }

  val testcase1 = Array[Expense](
    new Expense("Coffee", 450),
    new Expense("Cake", 350) )
  val testcase2 = Array[Expense]()
  assert (total (testcase1) == 800)
  assert (total (testcase2) == 0)

  // Exercise 5

  def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean) :Boolean = {
    @annotation.tailrec
    def f(list: List[A], ordered: (A, A) => Boolean) : Boolean = list match {
      case a::b::tail => ordered(a, b) && f(b::tail, ordered)
      case _ => true
    }
    f(as.toList, ordered)
  }

  // some tests (uncomment)

  assert ( isSorted (Array(1,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  assert (!isSorted (Array(6,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  assert (!isSorted (Array(1,2,3,4,5,1), (a: Int, b: Int)=> a <= b))
  assert (isSorted (Array("emil", "michael"), (a: String, b: String)=> a <= b))
  assert (isSorted (Array(), (a: String, b: String)=> a <= b))

  // Exercise 6

  def curry[A,B,C] (f: (A,B)=>C) : A => (B => C) = (x) => (y) => f(x, y)

  // test if it type checks by currying isSorted automatically

  def isSorted1[A]: Array[A] => ((A,A)=>Boolean) => Boolean = curry (isSorted _)

  // Exercise 7

  def uncurry[A,B,C] (f: A => B => C) : (A,B) => C = (x, y) => f (x) (y)

  def isSorted2[A] : (Array[A], (A,A) => Boolean) => Boolean = uncurry (curry (isSorted _))
  assert (isSorted2 (Array(), (a: String, b: String)=> a <= b))

  // Exercise 8

  def compose[A,B,C] (f: B => C, g: A => B) : A => C = x => f(g(x))
  assert (compose ((x: Int) => x+2, (x: Int) => x*2) (5) == 12)
}

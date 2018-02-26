// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

// this is how we do simple interactive testing

val l1 :Stream[Int] = Empty
val l2 :Stream[Int] = empty

val l3 :Stream[Int]= cons(1, cons(2, cons (3, empty)))

def fibs : Stream[Int] = {
  def f(x:Int)(y:Int) : Stream[Int] = cons(x, f(y)(y+x))
  f (0)(1)
}


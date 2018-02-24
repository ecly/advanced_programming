import fpinscala.laziness._
import fpinscala.laziness.Stream._

object Tests extends App {
  // Exercise 1
  assert (Stream.from(5).take(5).toList == List(5, 6, 7, 8, 9))
  assert (Stream.to(-1).take(5).toList == List(-1, -2, -3, -4, -5))

  // Exercise 3
  assert (naturals.take(1000000000).drop(41).take(4).toList == List(42, 43, 44, 45))

  // Exercise 4
  assert (naturals.takeWhile (_<1000000000).drop(100).take(2).toList == List(101, 102))

  // Exercise 5
  assert (naturals.forAll (_ < 5) == false)

  // Exercise 6
  assert (naturals.takeWhile2 (_<1000000000).drop(100).take(2).toList == List(101, 102))

  // Exercise 7
  assert (naturals.headOption2() == Some(1))
  assert (naturals.take(0).headOption2() == None)
}

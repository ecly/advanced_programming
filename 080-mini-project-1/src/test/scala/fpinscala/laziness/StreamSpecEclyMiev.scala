// Advanced Programming
// Emil Lynegaard (ecly)
// Michael Vesterli (miev)

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
//import stream01._ // uncomment to test the broken headOption implementation
//import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecEclyMiev extends FlatSpec with Checkers {

  import Stream._

  behavior of "headOption"

  // a scenario test:

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  it should "not force the tail of the stream" in {
    assert(Stream.from(0).headOption == Some(0))
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
    yield list2stream (la)

  implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
  // a property test:

  it should "return the head of the stream packaged in Some (02)" in check {
    // the implict makes the generator available in the context
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )
  }

  behavior of "take"

  it should "not force any heads" in {
    Stream.from(0).map(_ => throw new RuntimeException()).take(5)
  }

  it should "not force the n'th+1 value" in {
    val length = 100;
    val stream =
      Stream
        .from(0)
        .map(i => if (i >= length) { throw new RuntimeException() } else { i })
        .take(length);
    assert(stream.toList.length == length)
  }

  it should "return the same when calling take(n) once and twice" in check {
    Prop.forAll {
      (s: Stream[Int], n: Int) => {
        s.take(n).take(n).toList == s.take(n).toList
      }
    }
  }

  behavior of "drop"

  it should "drop additively" in check {
    val gen = for {
      s <- genNonEmptyStream[Int]
      n <- Gen.choose(0, 1000)
      m <- Gen.choose(0, 1000)
    } yield (s, n, m)
    Prop.forAll(gen) {
      case (s, n, m) => s.drop(n).drop(m).toList == s.drop(n+m).toList
    }
  }

  it should "not force any dropped elements" in {
    val n = 30;
    val stream =
      Stream
        .from(0)
        .map(i => if (i < n) { throw new RuntimeException() } else { i });
    assert(stream.take(2*n).drop(n).toList.length == n)
  }

  behavior of "map"

  it should "stay the same on the identity function" in check {
    Prop.forAll {
      (s: Stream[Int]) => s.map(identity).toList == s.toList
    }
  }

  behavior of "append"

  it should "not matter if converted to List before or after" in check {
    Prop.forAll {
      (s1: Stream[Int], s2: Stream[Int]) => s1.append(s2).toList == s1.toList ++ s2.toList
    }
  }

  it should "terminate when appending an infinite list" in check {
    val inf = Stream.from(0);
    Prop.forAll {
      (s: Stream[Int]) => { s.append(inf); true }
    }
  }

  it should "be unchanged if empty stream is appended" in check {
    Prop.forAll {
      (s: Stream[Int]) => s.append(Stream.empty).toList == s.toList
    }
  }
}


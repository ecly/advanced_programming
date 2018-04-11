// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
// Example solution for scala exercises using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package fpinscala.monoids
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

object MonoidSpec extends Properties("Monoids..") {

  import Monoid._

  // Exercise 4 (intro to the exercise)

  def associative[A :Arbitrary] (m: Monoid[A]) :Prop =
    forAll { (a1: A, a2: A, a3: A) =>
      m.op(m.op(a1,a2), a3) == m.op(a1,m.op(a2,a3)) } :| "associativity"

  def unit[A :Arbitrary] (m :Monoid[A]) =
    forAll { (a :A) => m.op(a, m.zero) == a } :| "right unit" &&
    forAll { (a :A) => m.op(m.zero, a) == a } :| "left unit"

  def monoid[A :Arbitrary] (m :Monoid[A]) :Prop = associative (m) && unit (m)

  property ("stringMonoid is a monoid") = monoid (stringMonoid)

  // Exercise 4: test intAddition, intMultiplication, booleanOr,
  // booleanAnd and optionMonoid.

  property ("intAddition is a monoid") = monoid(intAddition)
  property ("intMultiplication is a monoid") = monoid(intMultiplication)
  property ("booleanOr is a monoid") = monoid(booleanOr)
  property ("booleanAnd is a monoid") = monoid(booleanAnd)
  property ("optionMonoid is a monoid") = monoid[Option[Int]](optionMonoid)

  // Exercise 5

  def homomorphism[A :Arbitrary,B :Arbitrary]
    (ma: Monoid[A]) (f: A => B) (mb: Monoid[B]) =
      forAll { (a1: A, a2: A) => f(ma.op(a1, a2)) == mb.op(f(a1), f(a2)) }

  def isomorphism[A :Arbitrary, B :Arbitrary]
    (ma: Monoid[A]) (fa: A => B) (mb: Monoid[B]) (fb: B => A) =
      homomorphism (ma) (fa) (mb) && homomorphism (mb) (fb) (ma)

  property ("stringMonoid and listMonoid[Char] are isomorphic") =
    isomorphism (stringMonoid) (s => s.toList) (listMonoid) (l => l.mkString)

  // Exercise 6

  property ("booleanOr and booleanAnd are isomorphic") =
    isomorphism (booleanOr) (!_) (booleanAnd) (!_)

  // Exercise 7 (the testing part)

  property ("productMonoid is a monoid") =
    monoid(productMonoid (optionMonoid[Int]) (listMonoid[String]))
}

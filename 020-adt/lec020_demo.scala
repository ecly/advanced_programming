// Demo 01: ListInt

trait ListInt
      case object Nil extends ListInt
      case class Cons(head:Int, tail: ListInt) extends ListInt

val x:ListInt = Nil
val y:ListInt = Cons(3,x)
val z:ListInt = Cons(2,y)

trait ListDouble
      case object Nil extends ListDouble
      case class Cons(head:Double, tail: ListDouble) extends ListDouble


 trait ListString
      case object Nil extends ListString
      case class Cons(head:String, tail: ListString) extends ListString


//Demo:  List[T]
trait List[+T]
case object  Nil extends List[Nothing]
case class Cons[+T] (head: T, tail: List[T]) extends List[T]

//Quiz: TreeInt
trait TreeInt
case object Nil extends TreeInt
case class Leaf (a:Int) extends TreeInt
case class Node (left: TreeInt, right:TreeInt) extends TreeInt

val x: TreeInt = Nil
val y: TreeInt = Leaf(2)
val z: TreeInt = Leaf (3)
val w: TreeInt = Node (y,z)


//Quiz: ExprInt
trait ExprInt
case class Const (c:Int) extends ExprInt
case class Add(lhs:ExprInt, rhs:ExprInt) extends ExprInt
case class Prod(lhs:ExprInt,rhs:ExprInt) extends ExprInt
//demo: check Add(Const(2), Const(3)) is indeed a subtype of ExprInt

//Patter matching

trait ListInt
case object Nil extends ListInt
case class Cons(head:Int, tail: ListInt) extends ListInt

val z:ListInt = Cons(1,Cons(2, Cons(3,Nil)))
println(z match {case Cons(hd,_) => hd})

def sum(ints:ListInt):Int = ints match {
  case Nil => 0
  case Cons(hd,tl) => hd + sum(tl)
}



//Quiz: ExprInt
trait ExprInt
case class Const (c:Int) extends ExprInt
case class Add(lhs:ExprInt, rhs:ExprInt) extends ExprInt
case class Mult(lhs:ExprInt,rhs:ExprInt) extends ExprInt

val e=Mult(Add(Const(2), Const(3)), Const(4))
def eval(e:ExprInt):Int= e match{
  case Const(c) =>c
  case Add(lhs,rhs) => eval(lhs)+eval(rhs)
  case Mult(lhs,rhs) => eval(lhs)*eval(rhs)
}

//demo: check Add(Const(2), Const(3)) is indeed a subtype of ExprInt


//Demo variadic functions
def printAll(x:Int*){for (i<-x) println(i)}

  //now you can try f(2,3,4)
printAll(2,3,4)
  //However, if you have s=1 to 10, you need to transform it to a Seq "1 to 10:_*"
printAll(1 to 20:_*)


//Demo primary constructor
  // Toy1's has field "shape", and it is immutable (like val)
class Toy1 (val shape:String){
  println(s"A $shape toy is constructed")
}

// Toy2 has field "shape", and it is mutable (like var)
class Toy2 (var shape:String){
  println(s"A $shape toy is constructed")
}

//Toy0 does not have field "shape", but "shape" can be used in the toplevel body
class Toy0 (shape:String){
  println(s"A $shape toy is constructed")
}

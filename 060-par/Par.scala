import java.util.concurrent._
import scala.language.implicitConversions

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

object Par {

  type Par[A] = ExecutorService => Future[A]
  def run[A] (s: ExecutorService) (a: Par[A]) : Future[A] = a(s)


  case class UnitFuture[A] (get: A) extends Future[A] {
    def isDone = true
    def get (timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel (evenIfRunning: Boolean) : Boolean = false
  }

  def unit[A] (a: A) :Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C] (a: Par[A], b: Par[B]) (f: (A,B) => C) : Par[C] =
    (es: ExecutorService) => {
      val af = a (es)
      val bf = b (es)
      UnitFuture (f(af.get, bf.get))
    }

  def fork[A] (a: => Par[A]) : Par[A] = es => es.submit(
    new Callable[A] { def call = a(es).get }
  )

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  // Exercise 1 (CB7.4)

  def asyncF[A,B] (f: A => B) : A => Par[B] = a => lazyUnit(f(a))

  // map is shown in the book

  def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
    map2 (pa,unit (())) ((a,_) => f(a))

  // Exercise 2 (CB7.5)

  def sequence[A] (ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight (unit(Nil:List[A])) ((v, acc) => map2 (v, acc) (_::_))

  // Exercise 3 (CB7.6)

  // this is shown in the book:

  // def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
  //   val fbs: List[Par[B]] = ps.map(asyncF(f))
  //   sequence(fbs)
  // }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = as.map(asyncF(v => if (f(v)) { Some(v) } else { None }))
    map (sequence(pars)) (l => l.flatten(v => v))
  }

  // Exercise 4: implement map3 using map2

  def map3[A,B,C,D] (pa :Par[A], pb: Par[B], pc: Par[C]) (f: (A,B,C) => D) :Par[D]  =  {
    def partialAB (a: A, b: B)(c: C): D = f(a, b, c)
    val partialCD: Par[C => D] = map2(pa, pb)((a, b) => partialAB(a, b))
    def applyFunc(fcd: C => D, c: C): D = fcd(c)
    map2(partialCD, pc)((c2d, c) => applyFunc(c2d, c))
  }

  // shown in the book

  // def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  // Exercise 5 (CB7.11)

  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] =
    map2(n,sequence(choices))((idx, l) => l(idx))

  def choice[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =
    choiceN (map (cond) (b => if (b) 0 else 1)) (List(t, f))

  // Exercise 6 (CB7.13)

  // def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] = es => choices(pa(es).get)(es)

  def choiceNviaChooser[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] = chooser(n)(choices)

  def choiceViaChooser[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =
    chooser(cond)(b => if (b) t else f)

  // Exercise 7 (CB7.14)

  def join[A] (a : Par[Par[A]]) :Par[A] = chooser (a) (identity _)

  // Yes:
  // def flatmap[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] = join (map (pa) (choices))
  //
  // join is equal to chooser with the identify function (as seen in its declaration),
  // and the same goes for the relation between List.flatten and List.flatmap

  class ParOps[A](p: Par[A]) {

  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
}

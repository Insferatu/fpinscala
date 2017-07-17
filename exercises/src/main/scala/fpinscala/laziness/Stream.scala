package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {

  def toList: List[A] = {
    @tailrec def step(stream: Stream[A], res: List[A]): List[A] = stream match {
      case Cons(h, t) => step(t(), h() :: res)
      case Empty => res
    }

    step(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n != 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  @tailrec final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()) -> t())
    case Empty => None
  }

  def take2(n: Int): Stream[A] = unfold(this -> n) {
    case (Cons(h, t), i) if i > 0 => Some(h() -> (t(), i - 1))
    case _ => None
  }

  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h() -> t())
    case _ => None
  }

  def zipWith[B,C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this -> s) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()) -> (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this -> s) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())) -> (t1(), t2()))
    case (Cons(h1, t1), _) => Some((Some(h1()), None) -> (t1(), Empty))
    case (_, Cons(h2, t2)) => Some((None, Some(h2())) -> (Empty, t2()))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = this.zipWith(s)(_ == _).forAll(b => b)

  def tails: Stream[Stream[A]] = unfold(this) {
    case s @ Cons(_, t) => Some(s -> t())
    case _ => None
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Cons(h1, t1) => t1().scanRight(z)(f) match {
      case s @ Cons(h2, _) => cons(f(h1(), h2()), s)
    }
    case _ => cons(z, Empty)
  }

  def scanRight2[B](z: => B)(f: (A, => B) => B): Stream[B] = foldRight(cons(z, Empty)) { (a, b) =>
    b match {
      case s @ Cons(h, _) => cons(f(a, h()), s)
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def fib(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, fib(n2, n1 + n2))
    }

    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }
  }

  val ones2: Stream[Int] = unfold(1)(s => Some(s -> s))

  def constant2[A](a: A): Stream[A] = unfold(a)(s => Some(s -> s))

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some(s -> (s + 1)))

  val fibs2: Stream[Int] = unfold((0, 1)){ case (n1, n2) => Some(n1 -> (n2, n1 + n2)) }
}
package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => size(l) + size(r) + 1
    case Leaf(_) => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => math.max(maximum(l), maximum(r))
    case Leaf(v) => v
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => math.max(depth(l), depth(r)) + 1
    case Leaf(_) => 0
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(v) => f(v)
  }

  def sizeFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maximumFold(t: Tree[Int]): Int = fold(t)(v => v)(math.max)

  def depthFold[A](t: Tree[A]): Int = fold(t)(_ => 0)(math.max(_, _) + 1)

  def mapFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

}

object CheckTree {
  import Tree._

  def main(args: Array[String]): Unit = {
    val t1: Tree[Int] = Branch(
      Branch(
        Leaf(5),
        Branch(
          Leaf(8),
          Branch(
            Leaf(12),
            Leaf(4)
          )
        )
      ),
      Branch(
        Leaf(10),
        Leaf(6)
      )
    )

    println(depth(t1))
    println(depthFold(t1))
  }
}
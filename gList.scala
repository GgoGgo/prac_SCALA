//
// List.scala
// Create by GgoGgo at 11/7/17
// Copyright 2017 GgoGgo, All rights reserved
//

// '+' at '+A' means A is variance annotation
// or can be understood with Set theory concept:
//    if x is subset of y, then [x] is subset of [y]
sealed trait gList[+A] // at here trait is also can be abstract class
case object Nil extends gList[Nothing] // this is kind of pattern matching
case class Cons[+A](head: A, tail: gList[A]) extends gList[A]

// companion object ob List trait
object gList {
  def sum(ints: gList[Int]): Int = ints match {
    case Nil        => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(doubles: gList[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(x,xs) => x * product(xs)
  }
  def tail[A](l: gList[A]): gList[A] = l match {
    case Nil        => Nil
    case Cons(x,xs) => xs
  }
  // also can be
//  def tail[A](l: gList[A]): gList[A] = {
//    drop[A](as, 1)
//  }
  def setHead[A](as: gList[A], a: A): gList[A] = {
    Cons(a, as)
  }
  def drop[A](l: gList[A], n: Int): gList[A] = {
    def go[A](as: gList[A], n: Int): gList[A] = as match {
      case Nil        => Nil
      case Cons(x,xs) =>  if (n <= 0) as
                          else go(xs, n-1)
    }
    go(l,n)
  }
  def dropWhile[A](l: gList[A], f: A => Boolean): gList[A] = {
    def go[A](as: gList[A], f: A => Boolean):gList[A] = as match {
      case Nil        => Nil
      case Cons(x,xs) =>  if (f(x)) go(xs, f)
                          else as
    }
    go(l, f)
  }
  def append[A](a1: gList[A], a2: gList[A]): gList[A] = a1 match {
    case Nil => a2
    case Cons(x,xs) => Cons(x, append(xs,a2))
  }
  def init[A](l: gList[A]): gList[A] = {
    def go[A](as: gList[A], acc: gList[A]): gList[A] = as match {
      case Nil => Nil
      case Cons(x,xs) =>  if (xs == Nil) acc
                          else go(xs, append(acc,gList(x)))
    }
    go(l, Nil)
  }

  // variadic function
  // A* means almost same as ... argument in c/c++
  // type of A* is 'Seq'
  def apply[A](as: A*): gList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))  // 'as.tail: _*' makes type of parameter to 'Seq', so can be passed to method
  }
}

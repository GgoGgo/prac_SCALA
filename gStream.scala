//
// gStream.scala
// Create by GgoGgo at 11/12/17
// Copyright 2017 GgoGgo, All rights reserved
//

package prac.scala.temp

sealed trait gStream[+A]  // aka lazy list
case object Empty extends gStream[Nothing]
case class Cons[+A](h: () => A, t: () => gStream[A]) extends gStream[A]

object gStream {
  // cons and empty is constructor for gStream
  def cons[A](h: => A, t: gStream[A]): gStream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }
  def empty: gStream[A] = Empty

  def apply[A](as: A*): gStream[A] = {
    if (as.isEmpty) empty else Cons(as.head, apply(as.tail: _*))
  }
}
//
// gStream.scala
// Create by GgoGgo at 11/12/17
// Copyright 2017 GgoGgo, All rights reserved
//

package prac.scala.temp

// aka lazy list
sealed trait gStream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h()) // have to force thunk to get data
  }
  def headOptions: Option[A] = {
    this.foldRight(None: Option[A])((h,_) => Some(h))
  }
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }
  def take(n: Int): gStream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => gStream.cons(h(), t().take(n-1))
  }
  def takeWhile(p: A => Boolean): gStream[A] = this match {
    case Cons(h,t) if(p(h())) => gStream.cons(h(),t().takeWhile(p))
    case _ => Empty
  }
  def takeWhiles(p: A => Boolean): gStream[A] =
    foldRight(gStream.empty[A])((a,b) =>
      if (p(a)) gStream.cons(a,b)
      else gStream.empty)
  // p is abbr of predicate
  def exist(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exist(p)
    case _ => false
  }
  def exists(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  def map[B](f: A => B): Option[B] = this match {
    
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {

  }
  def filter
  def append
}
case object Empty extends gStream[Nothing]
case class Cons[+A](h: () => A, t: () => gStream[A]) extends gStream[A]

object gStream {
  // cons and empty is constructor for gStream
  def cons[A](h: => A, t: gStream[A]): gStream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }
  def empty[A]: gStream[A] = Empty

  def apply[A](as: A*): gStream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
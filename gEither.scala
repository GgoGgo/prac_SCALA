//
// gEither.scala
// Create by GgoGgo at 11/11/17
// Copyright 2017 GgoGgo, All rights reserved
//

package prac.scala.temp

sealed trait Either[+E,+A]{
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(a) => Left(a)
    case Right(a) => Right(f(a))
  }
  // #1 when mapping to right type, you must promote the left type parameter to some supertype
  // to satisfy the +E
  // #2 it can't be implemented with map and getOrElse because there is no getOrElse here which peel for value
  def flatMap[EE >: E,B](f: A => Either[EE,B]): Either[EE,B] = this match {
    case Left(a) => Left(a)
    case Right(a) => f(a)
  }
  // B must be supertype of A because it will be used to build left value
  def orElse[EE >: E,B >: A](b: => Either[EE,B]): Either[EE,B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }
  def map2[EE >: E,B,C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] = for {
    aa <- this
    bb <- b
  } yield { f(aa,bb) }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(as: List[Double]): Either[String, Double] = as match {
    case Nil => Left("it's Nil")
    case _ => Right(as.sum/as.length)
  }
  // when want to get more specified information about error
  // like stacktrace
  def Try[A](a: A): Either[Exception, A] = {
    try Right(a)
    catch { case e: Exception => Left(e) }
  }
  def sequence[E,A](as: List[Either[E,A]]): Either[E,List[A]] = as match {
    case Nil => Right(Nil)
    case x::xs => x flatMap (xx => sequence(xs) map (xx::_) )
  }
  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] = as match {
    case Nil => Right(Nil)
    case x::xs => f(x) flatMap (xx => traverse(xs)(f) map (xx::_))
  }
  // more simpler version
  def traverse1[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] = as match {
    case Nil => Right(Nil)
    case x::xs => (f(x) map2 traverse(xs)(f))(_::_)
  }
}
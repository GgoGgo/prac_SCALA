//
// gEither.scala
// Create by GgoGgo at 11/11/17
// Copyright 2017 GgoGgo, All rights reserved
//

package prac.scala.temp

sealed trait Either[+E,+A]
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
    catch { case e: Exception => Left(e)}
  }
}
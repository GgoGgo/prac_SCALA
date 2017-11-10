//
// List.scala
// Create by GgoGgo at 11/10/17
// Copyright 2017 GgoGgo, All rights reserved
//

//import scala.{Option => _}

// it can't be loaded on scala repl
// because interpreter ends its evaluation with braces
// it brings the result which reports missing case class and case object
sealed trait gOption[+A] {
  def map[B](f: A => B): gOption[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => gOption[B]): gOption[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => gOption[B]): gOption[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }
  def filter(f: A => Boolean): gOption[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a)
                    else None
  }
}
case class Some[+A](get: A) extends gOption[A]
case object None extends gOption[Nothing]


object gOption {
  def mean(xs: Seq[Double]): gOption[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
}


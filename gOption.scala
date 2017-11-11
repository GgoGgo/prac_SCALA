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
  def flatMap[B](f: A => gOption[B]): gOption[B] = {
    map(f) getOrElse None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => gOption[B]): gOption[B] = {
    this map(Some(_)) getOrElse(ob)
  }
  // if => is in front of if expression, it evaluates if statement as Unit (unless else specified)
  def filter(f: A => Boolean): gOption[A] = this match {
    case Some(a) if (f(a)) => Some(a)
    case _ => None
  }
}
case class Some[+A](get: A) extends gOption[A]
case object None extends gOption[Nothing]


object gOption {
  def mean(xs: Seq[Double]): gOption[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))
  }
  // any function (e.g. math functions) can be transform to operate on gOption context
  def lift[A,B](f: A => B): gOption[A] => gOption[B] = _ map f
}




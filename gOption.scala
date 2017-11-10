//
// List.scala
// Create by GgoGgo at 11/10/17
// Copyright 2017 GgoGgo, All rights reserved
//

sealed trait gOption[+A]
case class Some[+A](get: A) extends gOption[A]
case object None extends gOption[Nothing]

def mean(xs: Seq[Double]): gOption[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)
}

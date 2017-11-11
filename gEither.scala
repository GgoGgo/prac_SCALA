//
// gEither.scala
// Create by GgoGgo at 11/11/17
// Copyright 2017 GgoGgo, All rights reserved
//

package prac.scala.temp

sealed trait Either[+E,+A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
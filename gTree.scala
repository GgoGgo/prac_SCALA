//
// List.scala
// Create by GgoGgo at 11/8/17
// Copyright 2017 GgoGgo, All rights reserved
//

sealed trait gTree[+A]
case class Leaf[A](value: A) extends gTree[A]
case class Branch[A](l: gTree[A], r: gTree[A]) extends gTree[A]


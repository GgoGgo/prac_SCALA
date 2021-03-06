//
// List.scala
// Create by GgoGgo at 11/8/17
// Copyright 2017 GgoGgo, All rights reserved
//

def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
  // this is kind of double loop
  // go2 is count for as1 and go1 is count for as2
  def go1(as1: List[A], as2: List[A]): Boolean = as2 match {
    case Nil => true
    case _ => if (as1.head == as2.head) go1(as1.tail, as2.tail)
              else false// exit
  }
  def go2(as1: List[A], as2: List[A]): Boolean = as1 match {
    case Nil => false
    case _ => if (go1(as1, as2)) true
              else go2(as1.tail, as2)
  }
  go2(sup, sub)
}

def main(args: Array[String]): Unit = {
  hasSubSequence(List(1, 2, 3, 4), List(4))
}
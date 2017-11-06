//
// IsSorted.scala
// Create by GgoGgo at 11/6/17
// Copyright 2017 GgoGgo, All rights reserved
//

object IsSorted {
  def check[A](as: Array[A], isOrd: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if      (n >= as.length)        true
      else if (isOrd(as(n-1), as(n))) loop(n+1)
      else                            false
    }
    loop(1)
  }
}
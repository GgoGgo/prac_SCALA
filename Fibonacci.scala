//
// Fibonacci.scala
// Create by GgoGgo at 11/6/17
// Copyright 2017 GgoGgo, All rights reserved
//

object Fibonacci {
  def getFibNumber(n: Int): Int = {
    if (n < 1) 0
    else {
      // scala does not stop with expression matching for return value type (actually it's not return type, it is value itself in scala)
      // unlike haskell, scala execute all expression untill it ends
      // so if this definition is not in [else] it will ignore exception handling which 'if' statement intended
      def loop(n: Int): Int = {
        if      (n == 1) 0
        else if (n == 2) 1
        else    (loop(n - 1) + loop(n - 2))
      }
      loop(n)
    }
  }
}
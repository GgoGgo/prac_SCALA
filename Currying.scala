//
// Currying.scala
// Create by GgoGgo at 11/6/17
// Copyright 2017 GgoGgo, All rights reserved
//

object Currying {
  // it is really similar to haskell :) god damn funny
  // recommended: https://en.wikipedia.org/wiki/Currying
  // currying is the technique of translating the evaluation of a function that takes multiple arguments (or a tuple of arguments) into evaluating a sequence of functions, each with a single argument. Currying is related to, but not the same as, partial application
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a,b))
  }
  // since [=>] is associates to the right
  // A => (B => C) can be written as
  // A => B => C
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a: A, b: B) => (f(a))(b) // it is same as f(a)(b), parenthesis is just for readability
  }
  // SCALA provides [compose] as a method on [Function1]
  // f compose g can be written as
  // g andthen f
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
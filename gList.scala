//
// gList.scala
// Create by GgoGgo at 11/7/17
// Copyright 2017 GgoGgo, All rights reserved
//

// '+' at '+A' means A is variance annotation
// or can be understood with Set theory concept:
//    if x is subset of y, then [x] is subset of [y]
sealed trait gList[+A] // at here trait is also can be abstract class
case object Nil extends gList[Nothing] // this is kind of pattern matching
case class Cons[+A](head: A, tail: gList[A]) extends gList[A]

// companion object ob List trait
object gList {
  def sum(ints: gList[Int]): Int = ints match {
    case Nil        => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(doubles: gList[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(x,xs) => x * product(xs)
  }

  def tail[A](l: gList[A]): gList[A] = l match {
    case Nil        => Nil
    case Cons(x,xs) => xs
  }
  // also can be
//  def tail[A](l: gList[A]): gList[A] = {
//    drop[A](as, 1)
//  }
  def setHead[A](as: gList[A], a: A): gList[A] = {
    Cons(a, as)
  }
  def drop[A](l: gList[A], n: Int): gList[A] = {
    def go(as: gList[A], n: Int): gList[A] = as match {
      case Nil        => Nil
      case Cons(x,xs) =>  if (n <= 0) as
                          else go(xs, n-1)
    }
    go(l,n)
  }
  // when a function definition contains multiple argument groups, type information flows
  // from left to right across these argument groups
  // so it can be called like
  // gList.dropWhile(gList(1, 2, 3 ,4))(x => x < 2)
  // not gList.dropWhile(gList(1, 2, 3 ,4))(x: Int => x <2)
  def dropWhile[A](l: gList[A])(f: A => Boolean): gList[A] = l match {
    case Cons(x,xs) if f(x) => dropWhile(xs)(f)
    case _ => l // actually it's only can be 'Nil'
  }
  def append[A](a1: gList[A], a2: gList[A]): gList[A] = a1 match {
    case Nil => a2
    case Cons(x,xs) => Cons(x, append(xs,a2))
  }
  def appendFoldR[A](a1: gList[A], a2: gList[A]): gList[A] = {
    foldRight(a1, a2)((a,b) => Cons(a,b))
  }
  // i think give type parameter explicitly lambda function pattern matches Nil type
  def concat[A](lists: gList[gList[A]]): gList[A] = {
    foldRight[gList[A], gList[A]](lists, Nil)(appendFoldR(_,_))
  }
  def init[A](l: gList[A]): gList[A] = {
    def go(as: gList[A], acc: gList[A]): gList[A] = as match {
      case Nil => Nil
      case Cons(x,xs) =>  if (xs == Nil) acc
                          else go(xs, append(acc,gList(x)))
    }
    go(l, Nil)
  }
  def length[A](l: gList[A]): Int = {
    foldRight(l, 0)((_,acc) => 1 + acc)
  }

  // variadic function
  // A* means almost same as ... argument in c/c++
  // type of A* is 'Seq'
  def apply[A](as: A*): gList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))  // 'as.tail: _*' makes type of parameter to 'Seq', so can be passed to method
  }
  def foldRight[A,B](l: gList[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
  def foldLeft[A,B](l: gList[A], z: B)(f: (B,A) => B): B = l match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
  }
  def foldRight_Stacksafe[A,B](l: gList[A], z: B)(f: (A,B) => B): B = {
    foldLeft(reverse2(l), z)((b,a) => f(a,b))   // this is stack safe because it has used foldleft for all created expression
  }
  def foldRight2[A,B](l: gList[A], z: B)(f: (A,B) => B): B = {
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
  }
  def foldLeft2[A,B](l: gList[A], z: B)(f: (B,A) => B): B = {
    // it's not trivial for first time reading
    // simple heuristic is to understand 'g' as a stack which input is a, understanding f as just operator and ignore what is f when thinking about transformation is really important
    // what f do is actually trivial, all transform is came from 'g'
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  }
  // doesn't need to refer type of x and y because type inference was finished with (ns, 0)
  def sum2(ns: gList[Int]) = {
    foldRight(ns, 0)((x,y) => x + y)
  }
  def product2(ns: gList[Double]) = {
    foldRight(ns, 1.0)(_ * _) // this notation can be used when type inference works for it
  }
  def sum3(ns: gList[Int]) = {
    foldLeft(ns, 0)(_+_)
  }
  def product3(ns: gList[Double]) = {
    foldLeft(ns, 1.0)(_*_)
  }

  def reverse1[A](ns: gList[A]): gList[A] = {
    foldRight[A,gList[A]](ns,Nil)((x,z) => append(z,gList(x)))
  }
  def reverse2[A](ns: gList[A]): gList[A] = {
    foldLeft[A,gList[A]](ns, Nil)((z,x) => append(gList(x),z))
  }
  def addOne(ints: gList[Int]): gList[Int] = ints match {
    case Nil => Nil
    case Cons(x,xs) => Cons(x+1, addOne(xs))
  }
  def d2String(ds: gList[Double]): gList[String] = ds match {
    case Nil => Nil
    case Cons(x,xs) => Cons(x.toString(), d2String(xs))
  }

  def map[A,B](as: gList[A])(f: A => B): gList[B] = as match {
    case Nil => Nil
    case Cons(x,xs) => Cons(f(x), map(xs)(f))
  }
  def filter[A](as: gList[A])(f: A => Boolean): gList[A] = as match {
    case Nil => Nil
    case Cons(x,xs) =>  if (f(x)) Cons(x, filter(xs)(f))
                        else filter(xs)(f)
  }
  def filterByFlatMap[A](as: gList[A])(f: A => Boolean): gList[A] = {
    flatMap(as)((a) => if (f(a)) gList(a) else Nil)
  }
  def flatMap[A,B](as: gList[A])(f: A => gList[B]): gList[B] = as match {
    case Nil => Nil
    case Cons(x,xs) => append(f(x), flatMap(xs)(f))
  }
  def head(l: gList[Int]): Int = l match {
    case Nil        => 0
    case Cons(x,xs) => x
  }
  def addGlist(as: gList[Int], bs: gList[Int]): gList[Int] = {
    if ((length(as) != length(bs)) || as == Nil) Nil
    else Cons(head(as) + head(bs),addGlist(tail(as),tail(bs)))
  }
  def zipWith[A,B](f: (A,B) => B,as: gList[A], bs: gList[B]): gList[B] = (as,bs) match {
    case (Cons(x,xs), Cons(y,ys)) => Cons(f(x,y), zipWith(f, xs, ys))
    case (_,_) => Nil
  }
}

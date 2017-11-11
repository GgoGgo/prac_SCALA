//
// gTree.scala
// Create by GgoGgo at 11/8/17
// Copyright 2017 GgoGgo, All rights reserved
//

sealed trait gTree[+A]
case class Leaf[A](value: A) extends gTree[A]
case class Branch[A](l: gTree[A], r: gTree[A]) extends gTree[A]

object gTree {
  def size[A](t: gTree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }
  // naive implementation
//  def maximum(t: gTree[Int]): Int = {
//    def go(t: gTree[Int], m: Int): Int = t match {
//      case Leaf(v) => v max m
//      case Branch(l,r) => go(l, m) max go(r, m)
//    }
//    go(t, 0)
//  }
  // functional implementation
  def maximum(t: gTree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }
  // naive implementation
//  def depth[A](t: gTree[A]): Int = {
//    def go(t: gTree[A], depth: Int, mDepth: Int): Int = t match {
//      case Leaf(_) => depth max mDepth
//      case Branch(l, r) => go(l, depth+1, mDepth) max go(r, depth+1, mDepth)
//    }
//    go(t,0,0)
//  }
  // functional-native implementation
  def depth[A](t: gTree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }
  def map[A,B](t: gTree[A], f: A => gTree[B]): gTree[B] = t match {
    case Leaf(v) => f(v)
    case Branch(l,r) => Branch(map(l,f), map(r,f))
  }

  def fold[A,B](t: gTree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  def sizeFold[A](t: gTree[A]): Int = {
    fold(t)(_=>1)(_+_)
  }
  def maximumFold(t: gTree[Int]): Int = {
    fold(t)(a=>a)(_ max _)
  }
  def depthFold[A](t: gTree[A]):Int = {
    fold(t)(_=>0)((a,b) => 1 + a max b)
  }
  def mapFold[A,B](t: gTree[A], f: A => gTree[B]): gTree[B] = {
    fold(t)(f)((a,b) => Branch(a,b))
  }
}
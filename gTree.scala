//
// List.scala
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
  def maximum(t: gTree[Int]): Int = {
    def go(t: gTree[Int], m: Int): Int = t match {
      case Leaf(v) => v max m
      case Branch(l,r) => go(l, m) max go(r, m)
    }
    go(t, 0)
  }
  def depth[A](t: gTree[A]): Int = {
    def go(t: gTree[A], depth: Int, mDepth: Int): Int = t match {
      case Leaf(_) => depth max mDepth
      case Branch(l, r) => go(l, depth+1, mDepth) max go(r, depth+1, mDepth)
    }
    go(t,0,0)
  }
  def map[A,B](t: gTree[A], f: A => B): gTree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l,f), map(r,f))
  }
}
package week3

object intlist {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}

trait List[T]
class Cons[T](val head: T, val tail: List[T]) extends List[T]
class Nil[T] extends List[T]
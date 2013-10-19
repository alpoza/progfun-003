package week4


trait MyList[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): MyList[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: MyList[T]) extends MyList[T] {
  def isEmpty: Boolean = false
}

object MyNil extends MyList[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

}

object List {
  def apply[T](x: T, y: T): MyList[T] = new Cons(x, new Cons(y, MyNil))
  def apply[T]() = MyNil
}

object test {
  val x: MyList[String] = MyNil
//  wrong scala version?
//  def f(xs: MyList[NonEmpty], x: Empty) = xs prepend x
}


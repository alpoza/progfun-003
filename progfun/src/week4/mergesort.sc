package week4

import scala.math.Ordering

object mergesort {
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }                                               //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]

  val nums = List(2, -4, 5, 7, -1, 33)            //> nums  : <error> = List(2, -4, 5, 7, -1, 33)
  val fruits = List("orange", "apple", "pineapple", "banana")
                                                  //> fruits  : <error> = List(orange, apple, pineapple, banana)
  msort(nums)                                     //> res0: <error> = List(-4, -1, 2, 5, 7, 33)
  msort(fruits)                                   //> res1: <error> = List(apple, banana, orange, pineapple)
}
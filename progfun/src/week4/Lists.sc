
object Lists {
  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }                                               //> last: [T](xs: List[T])T

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }                                               //> init: [T](xs: List[T])List[T]

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }                                               //> concat: [T](xs: List[T], ys: List[T])List[T]

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => reverse(ys) ++ List(y)
  }                                               //> reverse: [T](xs: List[T])List[T]

  def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)
                                                  //> removeAt: [T](n: Int, xs: List[T])List[T]

  def merge(xs : List[Int], ys: List[Int]): List[Int] =
  	xs match {
  		case Nil => ys
  		case x :: xs1 =>
  			ys match {
  				case Nil => xs
  				case y :: ys1 =>
  					if (x < y) x :: merge(xs1, ys)
  					else y :: merge(xs, ys1)
  			}
  	}                                         //> merge: (xs: List[Int], ys: List[Int])List[Int]

  
  val l = List(2, 4, 120, 123, 3, 1, 302)         //> l  : List[Int] = List(2, 4, 120, 123, 3, 1, 302)
  last(l)                                         //> res0: Int = 302
  init(l)                                         //> res1: List[Int] = List(2, 4, 120, 123, 3, 1)
  concat(l, l)                                    //> res2: List[Int] = List(2, 4, 120, 123, 3, 1, 302, 2, 4, 120, 123, 3, 1, 302
                                                  //| )
  reverse(l)                                      //> res3: List[Int] = List(302, 1, 3, 123, 120, 4, 2)
}
package week5

object asd {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	List(1,2,3)                               //> res0: List[Int] = List(1, 2, 3)
	val nums = List(2, -4, 5, 7, 1)           //> nums  : List[Int] = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")
                                                  //> fruits  : List[String] = List(apple, pineapple, orange, banana)

  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)
  }                                               //> squareList: (xs: List[Int])List[Int]

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)                           //> squareList2: (xs: List[Int])List[Int]

  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
  }                                               //> posElems: (xs: List[Int])List[Int]

  def posElems2(xs: List[Int]): List[Int] =
    xs filter (x => x > 0)                        //> posElems2: (xs: List[Int])List[Int]

  xs filter (x => x == 0)
}

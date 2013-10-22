package week5

object reductionoflists {
  val nums = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 3, 4, 23, 54)
                                                  //> nums  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 3, 4, 23, 54)

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sum(ys)
  }                                               //> sum: (xs: List[Int])Int

  val s0 = sum(nums)                              //> s0  : Int = 129

  def sum1(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x + y)
                                                  //> sum1: (xs: List[Int])Int
  val s1 = sum1(nums)                             //> s1  : Int = 129
  def product(xs: List[Int]) = (1 :: xs) reduceLeft ((x, y) => x * y)
                                                  //> product: (xs: List[Int])Int
  val p0 = product(nums)                          //> p0  : Int = 1113396224

  def sum3(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)
                                                  //> sum3: (xs: List[Int])Int
  val s3 = sum3(nums)                             //> s3  : Int = 129

  def sum4(xs: List[Int]) = (xs foldLeft 0)(_ + _)//> sum4: (xs: List[Int])Int
  val s4 = sum4(nums)                             //> s4  : Int = 129

  def product1(xs: List[Int]) = (xs foldLeft 1)(_ * _)
                                                  //> product1: (xs: List[Int])Int
  val p1 = product1(nums)                         //> p1  : Int = 1113396224

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys)(_ :: _)                     //> concat: [T](xs: List[T], ys: List[T])List[T]

  concat(nums, nums)                              //> res0: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 3, 4, 23, 54, 1, 2, 3, 4, 
                                                  //| 5, 6, 7, 8, 9, 3, 4, 23, 54)
  
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( f(_)::_ )           //> mapFun: [T, U](xs: List[T], f: T => U)List[U]
  
  mapFun[Int, Int](nums, x => x + 1)              //> res1: List[Int] = List(2, 3, 4, 5, 6, 7, 8, 9, 10, 4, 5, 24, 55)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((_, sum) => sum + 1)         //> lengthFun: [T](xs: List[T])Int
    
    lengthFun(nums)                               //> res2: Int = 13
}
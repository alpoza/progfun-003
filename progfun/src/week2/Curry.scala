package week2

object Curry {
  def fact(x: Int): Int =
    if (x <= 1) 1
    else x * fact(x - 1)

  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    sumF
  }

  def sumInt = sum(x => x)
  def sumCubes = sum(x => x * x * x)
  def sumFactorials = sum(fact)

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }
  
  def fact2(n: Int) = product(x => x)(1, n)
  
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }
  
  def mproduct(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)
  
  def main(args: Array[String]) {
	  println(sumCubes(1, 10) + sumFactorials(10, 20))
	  println(product(x => x * x)(3, 4))
	  println(fact(5))
	  println(mproduct(x => x * x)(3, 4))
  }
}
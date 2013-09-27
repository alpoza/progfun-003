package week2

object HigherOrder {
  def sumInts(a: Int, b: Int): Int =
    if (a > b) 0 else a + sumInts(a + 1, b)

  def cube(x: Int): Int = x * x * x

  def sumCubes(a: Int, b: Int): Int =
    if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

  def fact(x: Int): Int =
    if (x <= 1) 1
    else x * fact(x - 1)

  def sumFactorials(a: Int, b: Int): Int =
    if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)

  // they are all basically the same...

  // ------------------- now with higher order functions

  def id(x: Int): Int = x

  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)

  def sumInts2(a: Int, b: Int) = sum(id, a, b)
  def sumCubes2(a: Int, b: Int) = sum(cube, a, b)
  def sumFactorials2(a: Int, b: Int) = sum(fact, a, b)

  // now with anonymous functions!

  def sumInts3(a: Int, b: Int) =
    sum(x => x, a, b)

  def sumCubes3(a: Int, b: Int) =
    sum(x => x * x * x, a, b)
}

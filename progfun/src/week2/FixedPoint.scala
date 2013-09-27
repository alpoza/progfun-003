package week2

object FixedPoint {
  // f(x) = x
  // x, f(x), f(f(x)), f(f(f(f(x)))), ...

  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) =
    math.abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
//      println("guess = " + guess)
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  
  def sqrt(x: Double) =
    fixedPoint(y => (y + x / y) / 2)(1)
    
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
  
  def sqrt2(x: Double) =
    fixedPoint(averageDamp(y => x / y))(1)

  def main(args: Array[String]) {
	  println(fixedPoint(x => 1 + x/2)(1))
	  println(sqrt(4))
	  println(sqrt2(4))
  }
}
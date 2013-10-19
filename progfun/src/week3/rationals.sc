object rationals {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	val x = new Rational(1, 2)                //> x  : Rational = 1/2
	x.numer                                   //> res0: Int = 1
	x.denom                                   //> res1: Int = 2
	
	def addRational(r: Rational, s: Rational): Rational = {
		new Rational(
			r.numer * s.denom + s.numer * r.denom,
			r.denom * s.denom
		)
	}                                         //> addRational: (r: Rational, s: Rational)Rational
	
	def makeString(r: Rational): String =
		r.numer + "/" + r.denom           //> makeString: (r: Rational)String
	
	makeString(addRational(new Rational(1, 2), new Rational(2, 3)))
                                                  //> res2: String = 7/6
  
  val y = new Rational(2, 3)                      //> y  : Rational = 2/3
  x + y                                           //> res3: Rational = 7/6
  
  val a = new Rational(1, 3)                      //> a  : Rational = 1/3
  val b = new Rational(5, 7)                      //> b  : Rational = 5/7
  val c = new Rational(3, 2)                      //> c  : Rational = 3/2
  a.numer                                         //> res4: Int = 1
  a.denom                                         //> res5: Int = 3
  a - b - c                                       //> res6: Rational = -79/42
  b + b                                           //> res7: Rational = 10/7
  a - b                                           //> res8: Rational = 8/-21
  a max b                                         //> res9: Rational = 5/7
  //x.add(y).mul(z)
  
  //val strange = new Rational(1, 0)
  //strange.add(strange)
  
  new Rational(2)                                 //> res10: Rational = 2/1
	
	// precedence rules:
	// all letters
	// |
	// ^
	// &
	// < >
	// = !
	// :
	// + -
	// * / %
	// all other special chars
	// ((a + b) ^? (c ?^ d)) less ((a ==> b) | c)
	
}

class Rational(x: Int, y: Int) {
	require(y != 0, "denominator must not be zero")
	
	def this(x: Int) = this(x, 1)
	
	private def gcd(a: Int, b: Int): Int =
		if (b == 0) a
		else gcd(b, a % b)
	
	// private val g = gcd(x, y)
	
	def numer = x // / g
	def denom = y // / g
	
	def + (that: Rational) =
		new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom)
	
	def unary_- : Rational = new Rational(-numer, denom)
	
	def - (that: Rational) =
		this + -that
	
	def < (that: Rational) = numer * that.denom < that.numer * denom
	
	def max(that: Rational) = if (this < that) that else this
	
	override def toString = {
		val g = gcd(numer, denom)
		numer / g + "/" + denom / g
	}
		
}
package week2

object exercise {
	def fact(n: Int): Int = {
		def loop(acc: Int, n: Int): Int =
			if (n == 0) acc
			else loop(acc * n, n - 1)
		loop(1, n)
	}                                         //> fact: (n: Int)Int
	fact(4)                                   //> res0: Int = 24
}
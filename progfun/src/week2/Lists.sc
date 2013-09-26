package week2

import java.util.NoSuchElementException

object Lists {
  def sum(xs: List[Int]): Int =
    if (xs.isEmpty) 0
    else xs.head + sum(xs.tail)                   //> sum: (xs: List[Int])Int
    
  def max(xs: List[Int]): Int = {
  	def maxOf(x: Int, y: Int): Int = if (x == y || x > y) x else y
  	def findMax(x: Int, nums: List[Int]): Int =
  		if (nums.isEmpty) x
  		else findMax(maxOf(x, nums.head), nums.tail)
    if (xs.isEmpty) throw new NoSuchElementException()
    else findMax(0, xs)
   }                                              //> max: (xs: List[Int])Int
    
	sum(List(1, 5, 10))                       //> res0: Int = 16
	sum(List())                               //> res1: Int = 0
	sum(List(1))                              //> res2: Int = 1
	max(List(1, 34, 452, 10, 13434, 24, 0))   //> res3: Int = 13434
}
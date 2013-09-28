package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =  {
    def findParentheses(opens: Int, closes: Int, remaining: List[Char]): Boolean = {
      if (remaining.isEmpty) opens == closes
      else if (closes > opens) false
      else {
        val head = remaining.head
        findParentheses(opens + open(head), closes + close(head), remaining.tail)
      }
    }
    def open(char: Char): Int = if (char == '(') 1 else 0
    def close(char: Char): Int = if (char == ')') 1 else 0
    findParentheses(0, 0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else {
      countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
    }
}

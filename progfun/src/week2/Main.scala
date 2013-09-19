package week2

object Main {
  // calcs the value in some column c and row r in a pascal triangle.
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  // checks if the parentheses are balanced in any given expression
  def balance(chars: List[Char]): Boolean = {
    // given a count of opens and closes (of parantheses) and the rest of the string, recursively
    // calc it for next char.
    def findParentheses(opens: Int, closes: Int, remaining: List[Char]): Boolean = {
      // if there is no more chars, opens == closes will tell the truth.
      if (remaining.isEmpty) opens == closes
      // if at any time there is more closes than opens, it is not balanced
      else if (closes > opens) false
      else {
        // next chaaaaar!
        val head = remaining.head
        findParentheses(opens + open(head), closes + close(head), remaining.tail)
      }
    }
    def open(char: Char): Int = if (char == '(') 1 else 0
    def close(char: Char): Int = if (char == ')') 1 else 0
    findParentheses(0, 0, chars)
  }

  // so I can pass a String
  def balance(chars: String): Boolean = balance(chars.toList)

  // giving a quantity of money and a List of possible coins, return the change possibilities
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else {
      countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
    }
  }

  def main(args: Array[String]) {
    println(pascal(0, 2))
    println(pascal(1, 2))
    println(pascal(1, 3))
    println(pascal(6, 20))

    // should be true:
    println(balance(""))
    println(balance("(if (zero? x) max (/ 1 x))"))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)"))

    // should be false:
    println(balance(":-)"))
    println(balance("())("))

    println(countChange(4, List(1, 2)))
    println(countChange(4, List(1, 2, 3)))
    println(countChange(10, List(1, 2, 5, 10)))
  }
}
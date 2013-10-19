package week4

trait Expr {
  def eval: Int
}

class Number(n: Int) extends Expr {
  def eval: Int = n
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  def eval: Int = e1.eval + e2.eval
}

//object Expr2 {
//  def eval(e: Expr) = e match {
//    case Number(n) => n
//    case Sum(e1, e2) => eval(e1) + eval(e2)
//  }
//}
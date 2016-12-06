package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if (c == r || c == 0 || r == 1) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def parentheses(chars: List[Char]): List[Char] = chars filter (char => char == '(' || char == ')')

    def simpleBalance(opening: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) opening == 0
      else if (opening < 0) false
      else {
        chars.head match {
          case '(' => simpleBalance(opening + 1, chars.tail)
          case ')' => simpleBalance(opening - 1, chars.tail)
        }
      }
    }

    simpleBalance(0, parentheses(chars))
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && coins.nonEmpty) countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }
}

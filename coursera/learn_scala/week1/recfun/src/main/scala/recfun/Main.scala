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
      if (r == 0 || r == 1 || c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def travel(accu: Int, comp: Char, rem: List[Char]): Boolean = {
        def compare(char: Char): Int = if (char == '(') 1 else if (char == ')') -1 else 0

        if (rem.isEmpty && accu + compare(comp) == 0) true
        else if (accu + compare(comp) < 0) false
        else if (rem.isEmpty && accu + compare(comp) != 0) false
        else travel(accu + compare(comp), rem.head, rem.tail)
      }

      if (chars.isEmpty) false
      else travel(0, chars.head, chars.tail)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money > 0 && !coins.isEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else if (money == 0) 1
      else 0
    }
  }

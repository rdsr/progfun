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
    if (c == 0 || r == 0) 1
    else if (c == r) pascal(c - 1, r - 1)
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], openParen: Int): Boolean = 
      if (openParen < 0) false
      else if (chars.isEmpty) 
        openParen == 0
      else {
        val c = chars.head
        if (c == '(')
            balanceIter(chars.tail, openParen + 1)
        else if (c == ')')
            balanceIter(chars.tail, openParen - 1)
        else            
            balanceIter(chars.tail, openParen)
      }
    
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    if(money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
}

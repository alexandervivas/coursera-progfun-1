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
    def pascal(c: Int, r: Int): Int = if (r == 0) if (c == 0) 1 else 0 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def innerBalance(chars: List[Char], count: Int): Boolean =
        if (chars.isEmpty && count == 0) true
        else if (count < 0 || chars.isEmpty && count != 0) false
        else if (chars.head == '(') innerBalance(chars.tail, count + 1)
        else if (chars.head == ')') innerBalance(chars.tail, count - 1)
        else innerBalance(chars.tail, count)

      innerBalance(chars, 0)
    }


  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def innerCountChange(money: Int, coins: List[Int]) : Int =
        if (coins.isEmpty || money < 0) 0
        else if (money == 0) 1
        else innerCountChange(money, coins.tail) + innerCountChange(money - coins.head, coins)

      innerCountChange(money, coins.sortWith(_ > _))
    }

  }

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
      if(c == 0 || r == 0 || c >= r) 1
      else pascal(c-1, r-1) + pascal(c, r -1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def pb(chars: List[Char], opens: Int): Boolean = {
        if (chars.isEmpty) opens == 0
        else if(opens < 0) false
        else if (chars.head == '(') pb(chars.tail, opens + 1)
        else if (chars.head == ')') pb(chars.tail, opens - 1)
        else pb(chars.tail, opens)
      }
      pb(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

        def loop(acc: Int, coins: List[Int]): Int = {
          if (money <= 0 || acc > money || coins.isEmpty) 0
          else if (acc == money) 1
          else loop(acc + coins.head, coins) + loop(acc , coins.tail)
        }

      loop(0, coins)
    }
  }

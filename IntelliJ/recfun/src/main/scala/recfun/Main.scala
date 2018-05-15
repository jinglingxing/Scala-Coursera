package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parentheses Balancing")
    println(balance("(just an) example".toList))

    println("Counting change")
    val m = List(5,10,20,50,100,200,500)
    val l = List(1,2)
    println(countChange(300, m))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c==0 || c==r) 1 else {
        pascal(c, r-1) + pascal(c-1, r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def match1(chars: List[Char], open: Int): Boolean = {
        if (chars.isEmpty) open==0
        else if(chars.head == '(') match1(chars.tail, open + 1)
        else if (chars.head == ')') match1(chars.tail, open - 1)
        else match1(chars.tail, open)
      }
      match1(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money < 0) 0
      else if(money == 0) 1
      else if(money > 0 && coins.isEmpty) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }

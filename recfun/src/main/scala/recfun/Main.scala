package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 20) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if(c==0 || c==r)
      1
    else
      pascal(c-1,r-1)+pascal(c,r-1)
    

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    balanceTail(0,chars)
    
  def balanceTail(c: Int, chars: List[Char]): Boolean = 
    if(chars.isEmpty) {
      (c == 0)
    }
    else {
	    val newc = c + (if(chars.head=='(') 1 else if(chars.head==')') -1 else 0)
	    if(newc < 0)
	      false
	    else
	      balanceTail(newc, chars.tail)
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if(money==0)
	  1
	else if (coins.length == 0)
	  0
	else {
	    coins.map ( coin =>
	      if(money == coin)
	        1
	      else if(coin > money)
	        0
	      else
	        countChange(money-coin, coins.filter(_ >= coin))
	  	).sum
	}
}

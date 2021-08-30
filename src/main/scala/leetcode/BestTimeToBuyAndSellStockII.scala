package leetcode

object BestTimeToBuyAndSellStockII extends App {
  def maxProfit(prices: Array[Int]): Int = {
    if(prices.isEmpty || prices.length == 1) return 0
    (1 until prices.length).foldLeft(0){
      (acc, i) ⇒ if(prices(i) > prices(i - 1))
        acc + (prices(i) - prices(i-1))
      else acc
    }
  }
  println(maxProfit(Array(7,1,5,3,6,4)))
  println(maxProfit(Array(1, 2, 3, 4, 5)))
  println(maxProfit(Array(7,6,4,3,1)))
}

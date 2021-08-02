package leetcode

object FindTheMinNumOfFib extends App {
  def findMinFibonacciNumbers(k: Int): Int = {

    def fib(a: Int, b:Int): LazyList[BigInt] = a #:: fib(b, a + b)

    val lst = fib(1,1).takeWhile(_ <= k).toList
    if(lst.contains(k)) return 1
    def iter(i: BigInt, res: Int): Int = {
      if(i == 0) res
      else {
        val f = lst.filter(_ <= i).last
        iter(i - f, res +1)
      }
    }
    iter(k, 0)
  }
}

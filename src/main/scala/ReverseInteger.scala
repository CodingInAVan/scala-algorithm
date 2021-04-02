import scala.annotation.tailrec

object ReverseInteger extends App {
  /*
  Reverse Integer problem.
   */
  def reverse(x: Int): Int = {
    val str = x.toString
    val hasMinus = if(str(0) == '-') true else false
    val reverseNumbers = if (hasMinus) str.toArray.tail.reverse else str.toArray.reverse
    @tailrec
    def createNumber(idx: Int, d: Int, solution: Int) : Int = {
      if(idx >= reverseNumbers.length) return solution
      val sol = solution + reverseNumbers(idx).asDigit * d
      if(reverseNumbers(idx) != '0' && (sol < solution || sol < d)) return 0
      createNumber(idx+1, d / 10, sol)
    }
    val d = Math.pow(10, reverseNumbers.length-1).toInt
    val result = createNumber(0, d, 0)
    if (hasMinus) result * -1
    else result
  }

  println(reverse(123))
  println(reverse(-123))
  println(reverse(Int.MaxValue))
  println(reverse(Int.MinValue))
}

package leetcode

import scala.annotation.tailrec

object LargestNumber extends App {
  def largestNumber(nums: Array[Int]): String = {
    val value = nums.map(a ⇒ a.toString).sortWith((a,b) ⇒ a + b > b + a).mkString
    @tailrec
    def removeLeadingZero(str: String): String = {
      if(str.length > 1 && str.charAt(0) == '0') {
          removeLeadingZero(str.substring(1))
      } else str
    }
    removeLeadingZero(value)
  }
  println(largestNumber(Array(3,30,34,5,9)))
  println(largestNumber(Array(10,2)))
  println(largestNumber(Array(34323, 3432)))
  println(largestNumber(Array(0, 0)))
}

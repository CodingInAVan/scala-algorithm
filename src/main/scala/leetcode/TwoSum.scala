package leetcode

import scala.annotation.tailrec

object TwoSum extends App {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val cache = nums.zipWithIndex.toMap

    @tailrec
    def twoSumInner(idx: Int): (Int, Int) = {
      if (idx >= nums.length) (-1, -1)
      else if (cache.contains(target - nums(idx)) && idx != cache(target - nums(idx))) (idx, cache(target - nums(idx)))
      else twoSumInner(idx + 1)
    }

    Array(twoSumInner(0)._1, twoSumInner(0)._2)
  }

  val array = Array(1, 2, 3, 4)
  twoSum(array, 7).foreach(println)
}

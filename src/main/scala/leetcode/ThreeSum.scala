package leetcode

import scala.annotation.tailrec

object ThreeSum extends App {
  /*
  Leetcode 3Sum problem.
   */
  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val sortedNums = nums.sortWith(_ < _)

    @tailrec
    def iterate(current: Int, start: Int, end: Int, solutions: List[List[Int]]): List[List[Int]] = {
      if (start >= end) return solutions
      val sum = sortedNums(current) + sortedNums(start) + sortedNums(end)
      if (sum == 0) {
        val solution = sortedNums(start) :: sortedNums(current) :: sortedNums(end) :: Nil
        val newSolutions = solution :: solutions
        val findStartPos = findStart(start)
        iterate(current, findStartPos + 1, end, newSolutions)
      } else if (sum > 0) {
        iterate(current, start, end - 1, solutions)
      } else {
        iterate(current, start + 1, end, solutions)
      }
    }

    def findStart(start: Int): Int = {
      if (start + 1 < sortedNums.length - 1 && sortedNums(start) == sortedNums(start + 1)) {
        findStart(start + 1)
      }
      start
    }

    (for {
      i <- sortedNums.indices
      if i == 0 || (sortedNums(i - 1) != sortedNums(i))
    } yield iterate(i, i + 1, sortedNums.length - 1, List())).toList.flatten.distinct
  }

  val solutions = threeSum(Array(-1, 0, 1, 2, -1, -4))
  println(solutions)
}

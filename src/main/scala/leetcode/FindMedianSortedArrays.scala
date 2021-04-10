package leetcode

import scala.annotation.tailrec

object FindMedianSortedArrays extends App {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    val isEven = (nums1.length + nums2.length) % 2 == 0
    val mid = (nums1.length + nums2.length)/2 + 1

    def getSolution(solutions: List[Int]): Double = solutions match {
      case h1 :: h2 :: _ => if(isEven) (h1 + h2)/2.0 else h1
      case _ => 0.0
    }
    @tailrec
    def iterate(idx1: Int, idx2: Int, track: Int, solutions: List[Int]): Double = {
      if(track == mid){
        if(solutions.length < 2) return 0.0 // invalid.
        return getSolution(solutions)
      }
      if(nums1.length <= idx1)
        iterate(idx1, idx2+1, track+1, nums2(idx2):: solutions)
      else if(nums2.length <= idx2)
        iterate(idx1 + 1, idx2, track+1, nums1(idx1):: solutions)
      else if(nums1(idx1) < nums2(idx2)) {
        iterate(idx1 + 1, idx2, track+1, nums1(idx1) :: solutions)
      } else {
        iterate(idx1, idx2 + 1, track+1, nums2(idx2) :: solutions)
      }

    }
    if(nums1.isEmpty && nums2.length == 1) return nums2(0)
    if(nums2.isEmpty && nums1.length == 1) return nums1(0)
    iterate(0, 0, 0, List())
  }

  println(findMedianSortedArrays(Array(1,3), Array(2)))
}

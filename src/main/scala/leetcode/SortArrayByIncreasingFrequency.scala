package leetcode

object SortArrayByIncreasingFrequency extends App {
  def frequencySort(nums: Array[Int]): Array[Int] = {
    if(nums == null) null
    val countMap = nums.groupBy(x => x).toList.sortWith((x, x1) => {
      if(x._2.length < x1._2.length) true
      else if(x._2.length > x1._2.length) false
      else x._1 > x1._1
    })
    countMap.flatMap(x => x._2).toArray
  }

  val nums = Array(2,3,1,3,2)
  frequencySort(nums).foreach(println)
}

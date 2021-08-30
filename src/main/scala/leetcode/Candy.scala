package leetcode

object Candy extends App {
  def candy(ratings: Array[Int]): Int = {
    if(ratings.isEmpty) return -1
    if(ratings.length == 1) return 1
    val left = ratings.indices.foldLeft(List.empty[Int]){
      (l, e) ⇒ {
        if(l.nonEmpty && ratings(e) > ratings(e-1))
          (l.head + 1) :: l
        else
          1 :: l
      }
    }
    val right = left.reverse.zipWithIndex.foldRight(List.empty[Int]){
      (e, l) ⇒ {
        if(e._2 < ratings.length - 1 && ratings(e._2) > ratings(e._2 + 1))
          math.max(l.head + 1, e._1) :: l
        else
          e._1 :: l
      }
    }
    right.sum
  }
//  println(candy(Array()))
//  println(candy(Array(1)))
    println(candy(Array(1,0,2)))
//  println(candy(Array(1,2,2)))
    println(candy(Array(2,1,2,3,1,2))) // 11
//  println(candy(Array(1,3,2,2,1))) // 7
//  println(candy(Array(29,51,87,87,72,12))) // 12
}

package main.scala

import scala.annotation.tailrec

object TwoNumbers extends App {
  /*
  2 :: (4 :: (3 :: Nil))
  5 :: (6 :: (4 :: Nil))
   */
  def addTwoNumbers(l1: List[Int], l2: List[Int]): List[Int] = {
    @tailrec
    def innerHelper(l1: List[Int], l2: List[Int], carr: Int, solution: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, Nil) =>
        if(carr > 0) carr :: solution else solution
      case (h1::t1, Nil) =>
        val sum = addTwoNumbers(0, h1, carr)
        innerHelper(t1, Nil, sum._2, sum._1 :: solution)
      case (Nil, h2::t2) =>
        val sum = addTwoNumbers(0, h2, carr)
        innerHelper(t2, Nil, sum._2, sum._1 :: solution)
      case (h1::t1, h2::t2) =>
        if (h1 + h2 + carr > 9)
          innerHelper(t1, t2, 1, (h1 + h2 + carr) % 10 :: solution)
        else
          innerHelper(t1, t2, 0, (h1 + h2 + carr) % 10 :: solution)
    }
    def addTwoNumbers(a: Int, b: Int, c: Int) : (Int, Int) = {
      val s = a + b + c
      (s % 10, s / 10)
    }
    innerHelper(l1, l2, 0, List())
  }


  val input1 = List(9, 9, 9)
  val input2 = List(1)
  println(addTwoNumbers(input1, input2))
}

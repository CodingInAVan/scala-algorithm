package leetcode

import scala.annotation.tailrec

object LongestPalindromicSubstring extends App {
  def longestPalindrome(s: String): String = {
    def isPalindrome(s: String) : Boolean = {
      val charArr = s.toCharArray;
      val results = for{
        (i, j) <- charArr.take(s.length/2) zip charArr.reverse.take(s.length/2)
        if i == j
      } yield true
      results.length == s.length/2
    }
    @tailrec
    def iterate(str: String, start: Int, end: Int, solution: String): String = {
      if(start < 0 || end > s.length) {
        if(str.length > 1 && str(0) == str(str.length-1) && str.length > solution.length)
          return str
        else
          return solution
      }
      val substr = s.substring(start, end)
      if(substr.length == 1 || (substr.length > 1 && substr(0) == substr(substr.length-1) && substr.length > solution.length)) {
        val newSolution = if (substr.length > solution.length) substr else solution
        iterate(substr, start-1, end+1, newSolution)
      } else {
        solution
      }
    }
    if(isPalindrome(s)) return s

    val solutionList = (for {
      (c, i) <- s.zipWithIndex
    } yield iterate(c.toString, i, i + 1, "")).toList

    val solutionList2 = (for {
      (c, i) <- s.zipWithIndex
    } yield iterate(c.toString, i, i + 2, "")).toList

    List.concat(solutionList, solutionList2).sortWith(_.length > _.length).head
  }

  println(longestPalindrome("babad"))
}

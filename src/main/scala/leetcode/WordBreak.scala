package leetcode

object WordBreak extends App {
  def wordBreak(s: String, wordDict: List[String]): Boolean = {
    val dict = wordDict.toSet
    val res = (1 to s.length).foldLeft(List(0)){
      (acc, idx) ⇒ {
        if (acc.exists(a ⇒ dict.contains(s.substring(a, idx))))
          idx :: acc
        else acc
      }
    }
    res.head == s.length
  }
}

import scala.annotation.tailrec

object LongestSubstringWithoutRepeatingChars extends App {
  def lengthOfLongestSubstring(s: String): Int = {
    if(s == null || s.equals("")) return 0
    if(s.length == 1) return 1
    @tailrec
    def iterate(start: Int, end: Int, max: Int): Int = {
      if(start >= end || end > s.length) return max
      val substr = s.substring(start, end)
      val nextChar = if (end < s.length) {
        s.substring(end, end + 1)
      } else ""

      val dupIdx = substr.indexOf(nextChar)
      val nextStart = if(dupIdx > -1) {
        start + dupIdx + 1
      } else start
      val nextMax = if(substr.length > max) substr.length else max
      iterate(nextStart, end + 1, nextMax)
    }
    val result = iterate(0, 1, Int.MinValue)
    if(result == Int.MinValue) return 0
    result
  }

  println(lengthOfLongestSubstring("abcabcbb"))
  println(lengthOfLongestSubstring("pwwkew"))
  println(lengthOfLongestSubstring("au"))
}

package leetcode

object LetterCombinationsPhoneNumber extends App {
  def numPadAndChars = Map(
    '2' -> List("a","b","c"),
    '3' -> List("d","e","f"),
    '4' -> List("g","h","i"),
    '5' -> List("j","k","l"),
    '6' -> List("m","n","o"),
    '7' -> List("p","q","r","s"),
    '8' -> List("t","u","v"),
    '9' -> List("w","x","y","z")
  )

  def letterCombinations(digits: String): List[String] = {
    if(digits == null || digits.isEmpty) return List()
    val input = for {
      c <- digits.toCharArray
    } yield numPadAndChars(c)
    input.tail.foldLeft[List[String]](input.head)((a, b) ⇒
      a ++ (for {
        i ← a
        j ← b
      } yield i + j)
    ).filter(s ⇒ s.size == digits.length)
  }

  println(letterCombinations("2345"))
}

package leetcode

object LetterCombinationsPhoneNumber extends App {
  def numPadAndChars = Map(
    '2' -> List('a','b','c'),
    '3' -> List('d','e','f'),
    '4' -> List('g','h','i'),
    '5' -> List('j','k','l'),
    '6' -> List('m','n','o'),
    '7' -> List('p','q','r','s'),
    '8' -> List('t','u','v'),
    '9' -> List('w','x','y','z')
  )

  def letterCombinations(digits: String): List[String] = {
    if(digits == null || digits.isEmpty) return List()
    val input = for {
      c <- digits.toCharArray
    } yield numPadAndChars(c)

    def iterate(list: List[Char], idx: Int, solution: String, solutions: List[String]): List[String] = {
      if(idx == input.length || list.isEmpty) return solution :: solutions

      val lst = if(idx >= input.length - 1) List() else input(idx + 1)
      (for {
        c <- list
      } yield iterate(lst, idx + 1, solution + c, solutions)).flatten
    }

    iterate(input(0), 0, "", List())
  }

  println(letterCombinations("234"))
}

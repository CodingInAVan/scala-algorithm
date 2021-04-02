import scala.annotation.tailrec

object ContainerWithMostWater extends App {
  /*
  Solution for Container with Most Water problem
   */
  def maxArea(height: Array[Int]) : Int = {
    @tailrec
    def iterate(start: Int, end: Int, width:Int, solution: Int): Int = {
      if(start >= end) return solution
      val newSolution = math.max(math.min(height(start), height(end)) * width, solution)
      if(height(start) > height(end))
        iterate(start, end-1, width-1, newSolution)
      else
        iterate(start+1, end, width-1, newSolution)
    }
    iterate(0, height.length-1, height.length-1, 0)
  }
}

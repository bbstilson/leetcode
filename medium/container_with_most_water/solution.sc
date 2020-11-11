object Solution {

  def main(args: Array[String]): Unit = {
    println("hi")
  }

  def maxArea(height: Array[Int]): Int = {
    height.indices
      .foldLeft(0) {
        case (carry, left) =>
          val newMaxArea = ((left + 1) until height.size)
            .map { right => Math.min(height(left), height(right)) * (right - left) }
            .foldLeft(carry)(Math.max)

          Math.max(newMaxArea, carry)
      }
  }
}

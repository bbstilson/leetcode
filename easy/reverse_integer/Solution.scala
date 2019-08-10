import scala.util.Try

object Solution {
  def main(args: Array[String]): Unit = {
    println(reverse(123))
    println(reverse(-123))
    println(reverse(120))
  }

  def reverse(x: Int): Int = {
    val isNeg = x < 0
    val rev = Math
      .abs(x)
      .toString
      .reverse
    
    Try(rev.toInt)
      .toOption
      .map { n => if (isNeg) n * -1 else n }
      .getOrElse(0)
  }
}

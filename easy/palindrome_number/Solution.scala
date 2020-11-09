object Solution {

  def main(args: Array[String]): Unit = {
    println(isPalindrome(121))
  }

  def isPalindrome(x: Int): Boolean = {
    if (x < 0) {
      false
    } else {
      val str = x.toString
      str == str.reverse
    }
  }
}

def isPalindrome(x: Int): Boolean = {
  if (x < 0) {
    false
  } else {
    val str = x.toString
    str == str.reverse
  }
}

println(isPalindrome(121))

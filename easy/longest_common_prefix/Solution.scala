object Solution {

  def main(args: Array[String]): Unit = {
    println("no data for this one...")
  }

  def longestCommonPrefix(strs: Array[String]): String = {
    if (strs.isEmpty) {
      ""
    } else {
      _longestCommonPrefix(strs)
    }
  }

  def _longestCommonPrefix(strs: Array[String]): String = {
    val (_, smallestWordIdx) = strs
      .map(_.size)
      .zipWithIndex
      .minBy { case (size, _) => size }

    (strs(smallestWordIdx).size - 1) match {
      case n if n < 0 => ""
      case smallestWordLen => {
        val maybeLongest = (0 to smallestWordLen).takeWhile { n =>
          val curLetter = strs.head(n)
          strs.forall(w => w(n) == curLetter)
        }.lastOption

        maybeLongest match {
          case Some(idx) => strs.head.take(idx + 1)
          case None      => ""
        }
      }
    }
  }
}

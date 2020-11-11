import scala.util.Try

def reverse(x: Int): Int = {
  val isNeg = x < 0
  val rev = Math
    .abs(x)
    .toString
    .reverse

  Try(rev.toInt).toOption
    .map { n => if (isNeg) n * -1 else n }
    .getOrElse(0)
}

println(reverse(123))
println(reverse(-123))
println(reverse(120))

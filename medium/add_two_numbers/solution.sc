class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x

  def print: String = Option(next).map(_.print).map(x.toString() + _).getOrElse(x.toString())
}

object Solution {

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode =
    addTwoNumbersRec(l1, l2, 0)

  def addTwoNumbersRec(l1: ListNode, l2: ListNode, carry: Int): ListNode =
    (Option(l1), Option(l2)) match {
      case (Some(left), Some(right)) => {
        val sum = left.x + right.x + carry
        val nextCarry = sum / 10
        val nextSum = sum % 10
        new ListNode(nextSum, addTwoNumbersRec(left.next, right.next, nextCarry))
      }
      case (Some(left), None) => {
        val sum = left.x + carry
        val nextCarry = sum / 10
        val nextSum = sum % 10
        new ListNode(nextSum, addTwoNumbersRec(left.next, l2, nextCarry))
      }
      case (None, Some(right)) => {
        val sum = right.x + carry
        val nextCarry = sum / 10
        val nextSum = sum % 10
        new ListNode(nextSum, addTwoNumbersRec(l1, right.next, nextCarry))
      }
      case (None, None) if carry > 0 => new ListNode(carry, null)
      case _                         => null
    }
}

def mkLN(xs: List[Int]): ListNode = xs match {
  case Nil        => null
  case head :: tl => new ListNode(head, mkLN(tl))
}

val l1 = mkLN(List(2, 4, 3))
val l2 = mkLN(List(5, 6, 4))
// val l1 = mkLN(List(9, 9, 9, 9, 9, 9, 9))
// val l2 = mkLN(List(9, 9, 9, 9))

println(l1.print)
println(l2.print)
println(Solution.addTwoNumbers(l1, l2).print)

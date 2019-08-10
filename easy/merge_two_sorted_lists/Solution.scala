 class ListNode(var _x: Int = 0) {
   var next: ListNode = null
   var x: Int = _x
 }

object Solution {
  def main(args: Array[String]): Unit = {
    println("no data for this one...")
  }

  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
    (l1, l2) match {
      case (null, node) => node
      case (node, null) => node
      case (left, right) if (left.x <= right.x) => {
        left.next = mergeTwoLists(left.next, right)
        left
      }
      case (left, right) if (left.x > right.x) => {
        right.next = mergeTwoLists(left, right.next)
        right
      }
    }
  }
}

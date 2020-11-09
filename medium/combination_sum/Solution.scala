object Solution {

  def main(args: Array[String]): Unit = {
    println("no data for this one...")
  }

  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
    findCombinationSum(
      candidates.toList,
      target,
      List.empty,
      List.empty[List[Int]]
    ).reverse
  }

  def findCombinationSum(
    choices: List[Int],
    target: Int,
    stack: List[Int],
    acc: List[List[Int]]
  ): List[List[Int]] = {
    target match {
      case 0          => stack.reverse :: acc
      case t if t < 0 => acc
      case t => {
        choices.zipWithIndex
          .foldLeft(acc) {
            case (accum, (choice, idx)) =>
              val (_, myChoices) = choices.splitAt(idx)
              findCombinationSum(
                myChoices,
                t - choice,
                choice :: stack,
                accum
              )
          }
      }
    }
  }
}

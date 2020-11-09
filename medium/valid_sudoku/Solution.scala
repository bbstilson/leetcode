object Solution {
  import SolutionData._

  type Board = Array[Array[Char]]

  def main(args: Array[String]): Unit = {
    println(isValidSudoku(b1))
    println(isValidSudoku(b2))
  }

  def isValidSudoku(board: Board): Boolean = {
    checkHorizontal(board) &&
    checkHorizontal(board.transpose) &&
    checkBoxes(board)
  }

  def checkBoxes(board: Board): Boolean = {
    val boxCoords = for {
      x <- (0 to 8 by 3)
      y <- (0 to 8 by 3)
    } yield (x, y)

    boxCoords
      .map(isBoxValid(board))
      .reduce(_ && _)
  }

  def isBoxValid(board: Board)(xy: (Int, Int)): Boolean = {
    val (xP, yP) = xy
    val positions = for {
      x <- (xP until (xP + 3))
      y <- (yP until (yP + 3))
    } yield (x, y)

    val (isValid, _) = positions
      .map { case (x, y) => board(x)(y) }
      .filterNot(_ == '.')
      .foldLeft((true, Set.empty[Char])) {
        case ((valid, set), char) =>
          set.contains(char) match {
            case true  => (false, set)
            case false => (valid, set + char)
          }
      }

    isValid
  }

  def checkHorizontal(board: Board): Boolean = {
    board
      .forall { row =>
        val (isValid, _) = row
          .filterNot(_ == '.')
          .foldLeft((true, Set.empty[Char])) {
            case ((last, set), char) =>
              if (set.contains(char)) {
                (last && false, set)
              } else {
                (last && true, set + char)
              }
          }
        isValid
      }
  }
}

object SolutionData {

  val b1 = Array(
    Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
    Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
    Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
    Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
    Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
    Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
    Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
    Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
    Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
  )

  val b2 = Array(
    Array('8', '3', '.', '.', '7', '.', '.', '.', '.'),
    Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
    Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
    Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
    Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
    Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
    Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
    Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
    Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
  )
}

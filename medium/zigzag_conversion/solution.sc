import scala.annotation.tailrec

object Solution {

  sealed trait Direction {
    val currentLevel: Int
  }
  case class Up(currentLevel: Int) extends Direction
  case class Down(currentLevel: Int) extends Direction

  def convert(s: String, numRows: Int): String = {
    @tailrec
    def convertRec(
      cs: List[Char],
      levelMap: Map[Int, List[Char]],
      direction: Direction
    ): Map[Int, List[Char]] = cs match {
      case Nil => levelMap
      case head :: tl =>
        direction match {
          case Up(currentLevel) => {
            val nextDirection = if (currentLevel == 1) {
              if (numRows == 1) Down(1) else Down(currentLevel + 1)
            } else {
              Up(currentLevel - 1)
            }

            val nextLevelMap = updateMap(levelMap, currentLevel, head)
            convertRec(tl, nextLevelMap, nextDirection)
          }
          case Down(currentLevel) => {
            val nextDirection = if (currentLevel == numRows) {
              if (numRows == 1) Up(1) else Up(currentLevel - 1)
            } else {
              Down(currentLevel + 1)
            }

            val nextLevelMap = updateMap(levelMap, currentLevel, head)
            convertRec(tl, nextLevelMap, nextDirection)
          }
        }
    }

    val levelMap = convertRec(s.toCharArray().toList, Map.empty[Int, List[Char]], Down(1))
    levelMap.foreach(println)
    (1 to numRows).flatMap(levelMap.get).map(_.reverse.mkString).mkString
  }

  private def updateMap(
    levelMap: Map[Int, List[Char]],
    currentLevel: Int,
    c: Char
  ): Map[Int, List[Char]] = levelMap + (currentLevel -> levelMap
    .get(currentLevel)
    .map(c +: _)
    .getOrElse(List(c)))

}

val sol = Solution.convert("ABC", 1)
// val expected = "PAHNAPLSIIGYIR"
// val expected = "PINALSIGYAHRPI"
println(sol)
// println(sol == expected)

/** P A H N
  * APLSIIG
  * Y I R
  */

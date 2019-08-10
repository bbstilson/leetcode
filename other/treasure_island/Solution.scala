import scala.util.Try
import scala.collection.immutable.Queue

object Solution {
  val m1 = Array(
    Array('O','O','O','O'),
    Array('D','O','D','O'),
    Array('O','O','O','O'),
    Array('X','D','D','O'),
  )

  type OceanGrid = Array[Array[Char]]
  type Coord = (Int, Int)
  type History = Map[Coord, Coord]

  val START = (0, 0)

  def main(args: Array[String]): Unit = {
    println(findRoute(m1))
  }

  def findRoute(grid: OceanGrid): Int = {
    val (treasureCoord, history) = findRoute(grid, Queue((0, 0)), Map.empty[Coord, Coord])
    rebuildPath(history, treasureCoord).size
  }

  def findRoute(
    grid: OceanGrid,
    frontier: Queue[Coord],
    history: History
  ): (Coord, History) = {
    val (cur @ (curX, curY), nextFrontier) = frontier.dequeue

    grid(curX)(curY) match {
      case 'X' => (cur, history)
      case _ => {
        val nextChoices = getNextChoices(grid, cur, history)
        val nextF = nextChoices.foldLeft(nextFrontier)(_ enqueue _)
        val nextHistory = nextChoices
          .zipAll(Nil, (-1, -1), cur)
          .foldLeft(history)(_ + _)

        findRoute(grid, nextF, nextHistory)
      }
    }
  }

  def getNextChoices(
    grid: OceanGrid,
    coord: Coord,
    history: History
  ): List[Coord] = {
    val (x, y) = coord
    val choice = mkChoice(grid, history)

    List(
      choice(x - 1, y), // Up
      choice(x + 1, y), // Down
      choice(x, y - 1), // Left
      choice(x, y + 1)  // Right
    ).flatten
  }

  def mkChoice(grid: OceanGrid, history: History) = (x: Int, y: Int) => {
    Try(grid(x)(y))
      .toOption
      // Isn't rocks and we haven't been there before.
      .filterNot(_ == 'D' || history.get((x, y)).isDefined)
      .map(_ => (x, y))
  }

  def rebuildPath(map: History, coord: Coord): List[Coord] = {
    map.get(coord) match {
      case Some(next) if (next == START) => next :: Nil
      case Some(next) => coord :: rebuildPath(map, next)
      case None => Nil
    }
  }
}

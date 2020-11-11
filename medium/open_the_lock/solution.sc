class OpenLock(deadends: Set[String], target: String) {
  private var q = scala.collection.immutable.Queue.empty[(String, Int)]
  private var moves = deadends

  def openLock(): Int = _openLock("0000", 0)

  private def _openLock(currentPosition: String, depth: Int): Int = {
    if (currentPosition == target) {
      depth
    } else {
      Lock(currentPosition).nextMoves
        .filterNot(moves)
        .foreach { move =>
          moves = moves + move
          q = q.enqueue((move, depth + 1))
        }

      q.dequeueOption match {
        case Some(((nextMove, nextDepth), nextQueue)) => {
          q = nextQueue
          _openLock(nextMove, nextDepth)
        }
        case None => -1
      }
    }
  }
}

case class Lock(pos: String) {

  def nextMoves: List[String] = {
    val is = pos.split("").toList.map(_.toInt)
    val isWithIndex = is.zipWithIndex

    isWithIndex
      .flatMap {
        case (p, idx) =>
          List(
            (is.take(idx) :+ rotate(p, 1)) ++ is.drop(idx + 1),
            (is.take(idx) :+ rotate(p, -1)) ++ is.drop(idx + 1)
          )
      }
      .map(_.mkString)
  }

  private def rotate(i: Int, direction: Int) =
    i + direction match {
      case -1 => 9
      case 10 => 0
      case n  => n
    }
}

def openLock(deadends: Array[String], target: String): Int = {
  val game = new OpenLock(deadends.toSet, target)
  if (target == "0000") 0
  else if (deadends.contains("0000")) -1
  else game.openLock()
}

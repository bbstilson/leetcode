def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {
  implicit val courses = prerequisites
    .map(a => a.head -> a.last)
    .foldLeft(Map.empty[Int, List[Int]]) {
      case (map, (course, prereq)) =>
        map + (course -> map.get(course).map(prereq +: _).getOrElse(List(prereq)))
    }

  courses
    .map { case (course, _) => _canFinish(course, Set.empty[Int]) }
    .takeWhile(identity)
    .size == courses.size

}

def _canFinish(loc: Int, seen: Set[Int])(implicit edges: Map[Int, List[Int]]): Boolean = {
  edges.get(loc) match {
    case Some(prereqs) if seen.contains(loc) => false
    case Some(prereqs)                       => prereqs.forall(prereq => _canFinish(prereq, seen + loc))
    case None                                => true
  }
}

// println(canFinish(2, Array(Array(0, 1)))) // true
// println(canFinish(2, Array(Array(1, 0)))) // true
// println(canFinish(2, Array(Array(1, 0), Array(0, 1)))) // false
// println(canFinish(2, Array(Array(1, 0), Array(1, 2), Array(0, 1)))) // false

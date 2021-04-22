import $ivy.`com.github.pathikrit::better-files:3.9.1`

import better.files._
import better.files.Dsl._
import scala.util._

object Difficulty extends Enumeration {
  type Difficulty = Value
  val Easy, Medium, Hard, Other = Value
}
import Difficulty._

implicit object diffReader
    extends mainargs.TokensReader[Difficulty](
      "difficulty",
      strs => {
        val s = strs.head
        val difficultyStr = s.head.toUpper +: s.tail.toLowerCase()
        Try(Difficulty.withName(difficultyStr)) match {
          case Failure(ex)    => Left(ex.getMessage())
          case Success(value) => Right(value)
        }
      }
    )

@main
def main(
  difficulty: Difficulty,
  problem: String
): String = {
  val difficultyDir = difficulty.toString().toLowerCase()
  val problemDir = problem.split(" ").map(_.toLowerCase).mkString("_")
  val fullDir = pwd / s"$difficultyDir/$problemDir"

  mkdir(fullDir)
  insertProblemToReadme(difficulty.toString(), problem, fullDir.toString())

  val readme = (fullDir / "README.md").write(s"""# $problem\n""")
  val solution = (fullDir / "solution.sc").touch()

  s"amm -w .${solution.toString().stripPrefix(pwd.toString())}"
}

def insertProblemToReadme(difficulty: String, problem: String, fullDir: String): Unit = {
  val readme = pwd / "README.md"
  val newLines = readme.lines
    .foldLeft(List.empty[String]) {
      case (newLines, line) =>
        if (line.startsWith("###") && line.toLowerCase.contains(difficulty.toLowerCase())) {
          s"\n[$problem]($fullDir)" +: line +: newLines
        } else {
          line +: newLines
        }
    }
    .reverse ++ List("")

  readme.overwrite(newLines.mkString("\n"))
}

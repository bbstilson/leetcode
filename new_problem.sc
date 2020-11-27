import $ivy.`com.github.pathikrit::better-files:3.9.1`

import scopt._
import better.files._
import better.files.Dsl._

object Difficulty extends Enumeration {
  type Difficulty = Value
  val Easy, Medium, Hard, Other = Value

  implicit val readDiffculty = new Read[Difficulty.Difficulty] {

    def reads: String => Difficulty.Difficulty =
      (Difficulty.withName _).compose(_.capitalize)

    def arity: Int = 1
  }
}
import Difficulty._

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
  val solution = fullDir / "solution.sc"

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

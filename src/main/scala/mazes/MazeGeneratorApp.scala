package mazes

import mazes.models.Grid
import mazes.generators.BinaryTree._
import mazes.typeclasses.ShowOps._

object MazeGeneratorApp extends App {

  val startGrid = Grid.init(5, 5)
  val maze = binaryTree(startGrid)
  println(show(maze))
}

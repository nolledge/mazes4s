package mazes

import mazes.models.Grid
import mazes.generators.BinaryTree._
import mazes.generators.SideWinder._
import mazes.typeclasses.ShowOps._

object MazeGeneratorApp extends App {

  val startGrid = Grid.init(20, 20)

  val binTreeMaze = binaryTree(startGrid)
  val siewinderMaze = sideWinder(startGrid)

  println(show(binTreeMaze))
  println(show(siewinderMaze))
}

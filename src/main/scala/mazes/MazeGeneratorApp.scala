package mazes

import mazes.models.Grid
import mazes.generators.BinaryTree._
import mazes.generators.SideWinder._
import mazes.typeclasses.ShowOps._
import mazes.models.Dijkstra
import mazes.models.Point

object MazeGeneratorApp extends App:

  val startGrid = Grid.init(5, 5)

  val binTreeMaze = binaryTree(startGrid)
  val siewinderMaze = sideWinder(startGrid)

  println("Binary Tree Maze")
  println(show(binTreeMaze))
  println(
    Dijkstra.fromRoot(Grid.get(binTreeMaze)(Point(0, 0)).get, binTreeMaze)
  )
  println("Sidewinder Maze")
  println(show(siewinderMaze))

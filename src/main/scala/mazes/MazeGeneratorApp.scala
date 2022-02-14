package mazes

import mazes.models.Grid
import mazes.generators.BinaryTree._
import mazes.generators.SideWinder._
import mazes.typeclasses.ShowOps._
import mazes.models.Dijkstra
import mazes.models.Point

object MazeGeneratorApp extends App:

  val gridWidth = 50
  val gridHeight = 50

  val startGrid = Grid.init(gridWidth, gridHeight)
  val binTreeMaze = binaryTree(startGrid)
  val siewinderMaze = sideWinder(startGrid)

  val southWest = Grid.get(siewinderMaze)(Point(0, 0)).get
  val southEast = Grid.get(siewinderMaze)(Point(gridWidth - 1, 0)).get

  println("Binary Tree Maze")
  println(show(binTreeMaze))
  println("Sidewinder Maze")
  println(show(siewinderMaze))
  val shortestPath: List[Point] = Dijkstra
    .shortestPath(
      southWest,
      southEast,
      Dijkstra.dijkstraDistances(
        southWest,
        siewinderMaze
      )
    )
    .map(_.point)

  val mazeWithPath = shortestPath.foldLeft(siewinderMaze) {
    case (grid, point) =>
      Grid
        .get(grid)(point)
        .fold(grid)(c => Grid.update(grid, c.copy(isPath = true)))

  }
  println(show(mazeWithPath))

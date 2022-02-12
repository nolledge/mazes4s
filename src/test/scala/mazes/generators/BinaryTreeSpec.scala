package mazes.generators

import mazes.models.Grid._
import mazes.models.Grid
import mazes.models.Cell
import mazes.models.Direction._
import mazes.generators.BinaryTree._

class BinaryTreeSpec extends munit.FunSuite {
  val rows: Int = 5
  val columns: Int = 3
  val maxX = columns - 1
  val maxY = rows - 1

  test("should have a root node with two links") {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)
    val rootNode = maze.cells(maxY)(maxX)
    assert(rootNode.north.isEmpty)
    assert(rootNode.east.isEmpty)
    assert(rootNode.south.isDefined)
    assert(rootNode.west.isDefined)
  }
  test("have a northern corridor") {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)
    assert(
      maze
        .cells(maxY)
        .forall(_.north.map(_.hasLink).getOrElse(true))
    )
  }
  test("have a eastern corridor") {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)
    assert(maze.cells.forall(r => r(maxX).east.map(_.hasLink).getOrElse(true)))
  }
  test("a northern neighbor should itself have a southern neighbor") {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)
    val cellWithNorthernNeighbor =
      maze.cells.flatten.find(_.north.exists(_.hasLink)).get
    val neighbor = cellWithNorthernNeighbor.north.get

    val northernCell = maze
      .cells(neighbor.point.y)(neighbor.point.x)

    assert(northernCell.south.get.hasLink)
    assertEquals(northernCell.south.get.point, cellWithNorthernNeighbor.point)
  }

  test("eastmost column should not have eastern neighbors") {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)

    assert(maze.cells.forall(y => y(maxX).east.isEmpty))
  }
  test("southmost row should not have southern neighbors") {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)

    assert(maze.cells.head.forall(c => c.south.isEmpty))
  }
  test("have every link set bidirectional") {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)
    assert(
      maze.cells.forall(
        _.forall(c =>
          Cell
            .neighbors(c)
            .filter(_.hasLink)
            .forall { n =>
              val refCell = maze.cells(n.point.y)(n.point.x)
              val correctReference = n.point == refCell.point
              val isLinked = n.direction match {
                case North => refCell.south.exists(_.hasLink)
                case South => refCell.north.exists(_.hasLink)
                case East  => refCell.west.exists(_.hasLink)
                case West  => refCell.east.exists(_.hasLink)
              }
              correctReference && isLinked
            }
        )
      )
    )
  }
}

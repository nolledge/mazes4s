package mazes.generators

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import mazes.models.Grid._
import mazes.models.Grid
import mazes.models.Cell
import mazes.models.Direction._
import mazes.generators.BinaryTree._

class BinaryTreeSpec extends AnyFlatSpec with Matchers {
  val rows: Int = 5
  val columns: Int = 3
  val maxX = columns - 1
  val maxY = rows - 1

  "The binaryTreeAlg" should "should have a root node with two links" in {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)
    val rootNode = maze.cells(maxY)(maxX)
    rootNode.north.isEmpty shouldBe true
    rootNode.east.isEmpty shouldBe true
    rootNode.south.isDefined shouldBe true
    rootNode.west.isDefined shouldBe true
  }
  "The binaryTreeAlg" should "have a northern corridor" in {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)
    maze
      .cells(maxY)
      .forall(_.north.map(_.hasLink).getOrElse(true)) shouldBe true
  }
  "The binaryTreeAlg" should "have a eastern corridor" in {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)
    maze.cells.forall(r =>
      r(maxX).east.map(_.hasLink).getOrElse(true)
    ) shouldBe true
  }
  "The binaryTreeAlg" should "a northern neighbor should itself have a southern neighbor" in {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)
    val cellWithNorthernNeighbor =
      maze.cells.flatten.find(_.north.exists(_.hasLink)).get
    val neighbor = cellWithNorthernNeighbor.north.get

    val northernCell = maze
      .cells(neighbor.point.y)(neighbor.point.x)

    northernCell.south.get.hasLink shouldBe true
    northernCell.south.get.point shouldBe cellWithNorthernNeighbor.point
  }

  "The binaryTreeAlg" should "eastmost column should not have eastern neighbors" in {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)

    maze.cells.forall(y => y(maxX).east.isEmpty) shouldBe true
  }
  "The binaryTreeAlg" should "southmost row should not have southern neighbors" in {
    val grid = Grid.init(rows, columns)
    val maze = binaryTree(grid)

    maze.cells.head.forall(c => c.south.isEmpty) shouldBe true
  }
  "The binaryTreeAlg" should "have every link set bidirectional" in {
    val grid = Grid.init(rows, columns)
    println("init test")
    val maze = binaryTree(grid)
    maze.cells.forall(
      _.forall(c =>
        Cell
          .neighbors(c)
          .filter(_.hasLink)
          forall { n =>
            val refCell = maze.cells(n.point.y)(n.point.x)
            val correctReference = n.point == refCell.point
            val isLinked = n.direction match {
              case North => refCell.south.exists(_.hasLink)
              case South => refCell.north.exists(_.hasLink)
              case East  => refCell.west.exists(_.hasLink)
              case West  => refCell.east.exists(_.hasLink)
            }
            println(c)
            println(refCell)
            correctReference && isLinked
          }
      )
    ) shouldBe true
  }
}

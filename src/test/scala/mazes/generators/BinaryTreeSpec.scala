package mazes.generators

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import mazes.models.Grid._
import mazes.models.Grid
import mazes.generators.BinaryTree._

class BinaryTreeSpec extends AnyFlatSpec with Matchers {
  val mazeSize: Int = 3
  val maxIndex = mazeSize - 1
  "The binaryTreeAlg" should "should have a root node with two links" in {
    val grid = Grid.init(mazeSize, mazeSize)
    val maze = binaryTree(grid)
    val rootNode = maze.cells(maxIndex)(maxIndex)
    rootNode.north.isEmpty shouldBe true
    rootNode.east.isEmpty shouldBe true
    rootNode.south.isDefined shouldBe true
    rootNode.west.isDefined shouldBe true
  }
  "The binaryTreeAlg" should "have a northern corridor" in {
    val grid = Grid.init(mazeSize, mazeSize)
    val maze = binaryTree(grid)
    maze
      .cells(maxIndex)
      .forall(_.north.map(_.hasLink).getOrElse(true)) shouldBe true
  }
  "The binaryTreeAlg" should "have a eastern corridor" in {
    val grid = Grid.init(mazeSize, mazeSize)
    val maze = binaryTree(grid)
    maze.cells.forall(r =>
      r(maxIndex).east.map(_.hasLink).getOrElse(true)
    ) shouldBe true
  }
  "The binaryTreeAlg" should "a northern neighbor should itself have a southern neighbor" in {
    val grid = Grid.init(mazeSize, mazeSize)
    val maze = binaryTree(grid)
    val cellWithNorthernNeighbor =
      maze.cells.flatten.find(_.north.exists(_.hasLink)).get
    val neighbor = cellWithNorthernNeighbor.north.get

    val northernCell = maze
      .cells(neighbor.point.x)(neighbor.point.y)

    northernCell.south.get.hasLink shouldBe true
    northernCell.south.get.point shouldBe cellWithNorthernNeighbor.point
  }

  "The binaryTreeAlg" should "eastmost row should not have eastern neighbors" in {
    val grid = Grid.init(mazeSize, mazeSize)
    val maze = binaryTree(grid)

    maze.cells(maxIndex).forall(_.east.isEmpty) shouldBe true
  }
  "The binaryTreeAlg" should "southmost row should not have eastern neighbors" in {
    val grid = Grid.init(mazeSize, mazeSize)
    val maze = binaryTree(grid)

    maze.cells.forall(r => r(0).south.isEmpty) shouldBe true
  }
  "The binaryTreeAlg" should "not flip the maze" in {
    val grid = Grid.init(mazeSize, mazeSize)
    val maze = binaryTree(grid)

    maze.cells(0).forall(r => r.point.x == 0) shouldBe true
  }
}

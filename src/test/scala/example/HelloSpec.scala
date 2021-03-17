package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Hello._
import Hello.Neighbor._
import Hello.Cell._

class HelloSpec extends AnyFlatSpec with Matchers {
  val mazeSize: Int = 3
  val maxIndex = mazeSize - 1
  "The Maze generator" should "init a grid where one row has the same x coordinate" in {
    // init field with 3 * 3 cells
    val grid = Grid.init(mazeSize)

    grid.cells(0).forall(_.point.x == 0) shouldBe true
  }
  "The Maze generator" should "init a grid with the neighbors set correctly" in {
    // init field with 3 * 3 cells
    val grid = Grid.init(mazeSize)

    // first column should not have any western neighbors
    grid.cells(0).forall(c => c.west.isEmpty) shouldBe true
    // last column should not have any eastern neighbors
    grid.cells(maxIndex).forall(c => c.east.isEmpty) shouldBe true
    // bottom row should not have any southern neighbors
    grid.cells.forall(x => x(0).south.isEmpty) shouldBe true
    // top row should not have any northern neighbors
    grid.cells.forall(x => x(maxIndex).north.isEmpty) shouldBe true

    // sorrounded cell should have all the correct neighbors
    val cell = grid.cells(1)(1)
    cell.south.get shouldBe Neighbor(Point(1, 0), South, false)
    cell.north.get shouldBe Neighbor(Point(1, 2), North, false)
    cell.east.get shouldBe Neighbor(Point(2, 1), East, false)
    cell.west.get shouldBe Neighbor(Point(0, 1), West, false)
  }
  "The binaryTreeAlg" should "should have a root node with two links" in {
    val grid = Grid.init(mazeSize)
    val maze = binaryTree(grid)
    val rootNode = maze.cells(maxIndex)(maxIndex)
    rootNode.north.isEmpty shouldBe true
    rootNode.east.isEmpty shouldBe true
    rootNode.south.isDefined shouldBe true
    rootNode.west.isDefined shouldBe true
  }
  "The binaryTreeAlg" should "have a northern corridor" in {
    val grid = Grid.init(mazeSize)
    val maze = binaryTree(grid)
    maze
      .cells(maxIndex)
      .forall(_.north.map(_.hasLink).getOrElse(true)) shouldBe true
  }
  "The binaryTreeAlg" should "have a eastern corridor" in {
    val grid = Grid.init(mazeSize)
    val maze = binaryTree(grid)
    maze.cells.forall(r =>
      r(maxIndex).east.map(_.hasLink).getOrElse(true)
    ) shouldBe true
  }
  "The binaryTreeAlg" should "a northern neighbor should itself have a southern neighbor" in {
    val grid = Grid.init(mazeSize)
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
    val grid = Grid.init(mazeSize)
    val maze = binaryTree(grid)

    maze.cells(maxIndex).forall(_.east.isEmpty) shouldBe true
  }
  "The binaryTreeAlg" should "southmost row should not have eastern neighbors" in {
    val grid = Grid.init(mazeSize)
    val maze = binaryTree(grid)

    maze.cells.forall(r => r(0).south.isEmpty) shouldBe true
  }
  "The binaryTreeAlg" should "not flip the maze" in {
    val grid = Grid.init(mazeSize)
    val maze = binaryTree(grid)

    maze.cells(0).forall(r => r.point.x == 0) shouldBe true
  }
}

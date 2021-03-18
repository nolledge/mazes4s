package mazes.models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import mazes.models.Direction._

class GridSpec extends AnyFlatSpec with Matchers {

  val mazeSize: Int = 3
  val maxIndex = mazeSize - 1

  "The Grid" should "init a grid where one row has the same y coordinate" in {
    // init field with 3 * 3 cells
    val grid = Grid.init(mazeSize, mazeSize)

    grid.cells(0).forall(_.point.y == 0) shouldBe true
  }
  "The Grid" should "init a grid with the neighbors set correctly" in {
    // init field with 3 * 3 cells
    val grid = Grid.init(mazeSize, mazeSize)

    // bottom row should not have any southern neighbors
    grid.cells(0).forall(c => c.south.isEmpty) shouldBe true
    // top row should not have any norhtern neighbors
    grid.cells(maxIndex).forall(c => c.north.isEmpty) shouldBe true
    // western column should not have any eastern neighbors
    grid.cells.forall(y => y(0).west.isEmpty) shouldBe true
    // eastern column should not have any eastern neighbors
    grid.cells.forall(y => y(maxIndex).east.isEmpty) shouldBe true

    // sorrounded cell should have all the correct neighbors
    val cell = grid.cells(1)(1)
    cell.south.get shouldBe Neighbor(Point(1, 0), South, false)
    cell.north.get shouldBe Neighbor(Point(1, 2), North, false)
    cell.east.get shouldBe Neighbor(Point(2, 1), East, false)
    cell.west.get shouldBe Neighbor(Point(0, 1), West, false)
  }
}

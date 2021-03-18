package mazes.models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import mazes.models.Direction._

class GridSpec extends AnyFlatSpec with Matchers {

  val mazeSize: Int = 3
  val maxIndex = mazeSize - 1

  "The Grid" should "init a grid where one row has the same x coordinate" in {
    // init field with 3 * 3 cells
    val grid = Grid.init(mazeSize, mazeSize)

    grid.cells(0).forall(_.point.x == 0) shouldBe true
  }
  "The Grid" should "init a grid with the neighbors set correctly" in {
    // init field with 3 * 3 cells
    val grid = Grid.init(mazeSize, mazeSize)

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
}

package mazes.models

import mazes.models.Direction._

class GridSpec extends munit.FunSuite {

  val mazeSize: Int = 3
  val maxIndex = mazeSize - 1

  test("The Grid#init a grid where one row has the same y coordinate") {
    // init field with 3 * 3 cells
    val grid = Grid.init(mazeSize, mazeSize)

    assert(grid.cells(0).forall(_.point.y == 0))
  }

  test("The Grid#init a grid with the neighbors set correctly") {
    // init field with 3 * 3 cells
    val grid = Grid.init(mazeSize, mazeSize)

    // bottom row should not have any southern neighbors
    assert(grid.cells(0).forall(c => c.south.isEmpty))
    // top row should not have any norhtern neighbors
    assert(grid.cells(maxIndex).forall(c => c.north.isEmpty))
    // western column should not have any eastern neighbors
    assert(grid.cells.forall(y => y(0).west.isEmpty))
    // eastern column should not have any eastern neighbors
    assert(grid.cells.forall(y => y(maxIndex).east.isEmpty))

    // sorrounded cell should have all the correct neighbors
    val cell = grid.cells(1)(1)
    assertEquals(cell.south.get, Neighbor(Point(1, 0), South, false))
    assertEquals(cell.north.get, Neighbor(Point(1, 2), North, false))
    assertEquals(cell.east.get, Neighbor(Point(2, 1), East, false))
    assertEquals(cell.west.get, Neighbor(Point(0, 1), West, false))
  }
}

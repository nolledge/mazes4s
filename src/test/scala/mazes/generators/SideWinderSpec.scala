package mazes.generators

import mazes.models.Grid._
import mazes.models.Grid
import mazes.generators.SideWinder._

class SideWinderSpec extends munit.FunSuite {
  val mazeSize: Int = 3
  val maxIndex = mazeSize - 1
  test("should have a corridor in the northmost row") {
    val grid = Grid.init(mazeSize, mazeSize)
    val maze = sideWinder(grid)
    assert(
      maze
        .cells(maxIndex)
        .forall(c => c.north.isEmpty && c.east.map(_.hasLink).getOrElse(true))
    )
  }
}

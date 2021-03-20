package mazes.generators
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import mazes.models.Grid._
import mazes.models.Grid
import mazes.generators.SideWinder._

class SideWinderSpec extends AnyFlatSpec with Matchers {
  val mazeSize: Int = 3
  val maxIndex = mazeSize - 1
  "The SiedeWinder" should "have a corridor in the northmost row" in {
    val grid = Grid.init(mazeSize, mazeSize)
    val maze = sideWinder(grid)
    maze
      .cells(maxIndex)
      .forall(c =>
        c.north.isEmpty && c.east.map(_.hasLink).getOrElse(true)
      ) shouldBe true
  }
}

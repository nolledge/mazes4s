package mazes.generators

import mazes.models.Grid
import mazes.models.Cell
import mazes.models.Direction._
import scala.util.Random

object BinaryTree extends BidiUpdater {

  def binaryTree(g: Grid): Grid = {
    withBidiLink(
      Grid(
        rows = g.rows,
        columns = g.columns,
        cells = g.cells.map(r => r.map(tearWall))
      )
    )
  }

  // Fixme: implement direction
  def tearWall(c: Cell): Cell =
    c match {
      case Cell(_, _, _, None, None, _) => c
      case Cell(_, _, _, Some(_), None, _) =>
        Cell.linkCell(c, North)
      case Cell(_, _, _, None, Some(_), _) =>
        Cell.linkCell(c, East)
      case Cell(_, _, _, Some(_), Some(_), _) if Random.nextInt(2) == 0 =>
        Cell.linkCell(c, East)
      case Cell(_, _, _, Some(_), Some(_), _) =>
        Cell.linkCell(c, North)
    }

}

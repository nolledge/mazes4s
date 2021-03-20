package mazes.generators

import mazes.models.{Grid, Cell}
import mazes.models.Direction._
import scala.util.Random

object SideWinder extends BidiUpdater {

  // start at 0,0
  // throw dice 50/50
  //   go east addding field to the current run
  //   OR end run when going north or wall reached
  //   on end of run carve one of the walls from the
  def sideWinder(g: Grid): Grid = {
    withBidiLink(
      Grid(
        rows = g.rows,
        columns = g.columns,
        cells =
          g.cells.map(_.foldLeft((Vector.empty[Cell], Vector.empty[Cell])) {
            case ((row, run), cell) if cell.north.isEmpty =>
              (row :+ Cell.linkCell(cell, East), Vector.empty)
            case ((row, run), cell)
                if cell.east.isDefined && Random.nextInt(2) == 0 =>
              (row, run :+ Cell.linkCell(cell, East))
            case ((row, run), cell) =>
              (row ++ tearOneNorthern(run :+ cell), Vector.empty[Cell])
          }._1)
      )
    )
  }

  def tearOneNorthern(v: Vector[Cell]): Vector[Cell] = {
    val rIndex = Random.nextInt(v.length)
    v.updated(rIndex, Cell.linkCell(v(rIndex), North))
  }

}

package mazes.generators

import mazes.models._
import mazes.models.Direction._

trait BidiUpdater {

  def withBidiLink(g: Grid): Grid = {
    val updatedNeigbors = for {
      row <- g.cells
      cell <- row
      updatedCell <- setBidirectionalLink(g)(cell)
    } yield updatedCell

    updatedNeigbors.foldLeft(g) {
      case (acc, cell) => Grid.update(acc)(cell)
    }

  }

  private def setBidirectionalLink(g: Grid)(c: Cell): Vector[Cell] =
    Cell
      .neighbors(c)
      .filter(_.hasLink)
      .flatMap(p =>
        Grid
          .get(g)(p.point)
          .map(targetCell =>
            p.direction match {
              case North =>
                targetCell
                  .copy(south = targetCell.south.map(_.copy(hasLink = true)))
              case East =>
                targetCell
                  .copy(west = targetCell.west.map(_.copy(hasLink = true)))
              case West =>
                targetCell
                  .copy(east = targetCell.east.map(_.copy(hasLink = true)))
              case South =>
                targetCell
                  .copy(north = targetCell.north.map(_.copy(hasLink = true)))
            }
          )
      )

}

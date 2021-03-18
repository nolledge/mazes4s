package mazes.generators

import mazes.models.Grid
import mazes.models.Cell
import mazes.models.Neighbor
import mazes.models.Direction._

trait BidiUpdater {

  def withBidiLink(g: Grid): Grid =
    g.cells
      .flatMap(_.flatMap(bidiUpdates(g)))
      .groupBy(_.point)
      .mapValues(_.toList)
      .flatMap {
        case (p, Nil)         => None
        case (p, head :: Nil) => Some(head)
        case (p, head :: tail) =>
          Some(
            head.copy(
              north = head.north
                .map(n => combineNeighbors(n, tail.flatMap(_.north))),
              east = head.east
                .map(e => combineNeighbors(e, tail.flatMap(_.east))),
              south = head.south
                .map(s => combineNeighbors(s, tail.flatMap(_.south))),
              west =
                head.west.map(w => combineNeighbors(w, tail.flatMap(_.west)))
            )
          )
      }
      .foldLeft(g)(Grid.update _)

  private def combineNeighbors(n: Neighbor, other: List[Neighbor]): Neighbor =
    n.copy(hasLink = n.hasLink || other.exists(_.hasLink))

  private def bidiUpdates(g: Grid)(c: Cell): Vector[Cell] =
    Cell
      .neighbors(c)
      .filter(_.hasLink)
      .flatMap(p =>
        Grid
          .get(g)(p.point)
          .map(targetCell =>
            p.direction match {
              case North =>
                targetCell.copy(south = targetCell.south.map(setLink))
              case East =>
                targetCell.copy(west = targetCell.west.map(setLink))
              case West =>
                targetCell.copy(east = targetCell.east.map(setLink))
              case South =>
                targetCell.copy(north = targetCell.north.map(setLink))
            }
          )
      )

  private def setLink(n: Neighbor): Neighbor = n.copy(hasLink = true)

}

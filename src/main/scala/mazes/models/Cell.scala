package mazes.models

import mazes.models.Direction._

final case class Cell(
    point: Point,
    west: Option[Neighbor],
    south: Option[Neighbor],
    north: Option[Neighbor],
    east: Option[Neighbor]
)

object Cell:

  def withUnlinkedNeighbors(
      point: Point,
      west: Option[Point],
      south: Option[Point],
      north: Option[Point],
      east: Option[Point]
  ): Cell =
    new Cell(
      point = point,
      west = west.map(c => Neighbor(c, West, hasLink = false)),
      south = south.map(c => Neighbor(c, South, hasLink = false)),
      north = north.map(c => Neighbor(c, North, hasLink = false)),
      east = east.map(c => Neighbor(c, East, hasLink = false))
    )

  def neighbors(c: Cell): Vector[Neighbor] =
    (c.west ++ c.south ++ c.north ++ c.east).toVector

  def isLinked(a: Cell, b: Cell): Boolean =
    neighbors(a).exists(n => n.hasLink && n.point == b.point)

  def linkCell(c: Cell, d: Direction): Cell =
    d match {
      case North => c.copy(north = c.north.map(_.copy(hasLink = true)))
      case South => c.copy(south = c.south.map(_.copy(hasLink = true)))
      case East  => c.copy(east = c.east.map(_.copy(hasLink = true)))
      case West  => c.copy(west = c.west.map(_.copy(hasLink = true)))
    }

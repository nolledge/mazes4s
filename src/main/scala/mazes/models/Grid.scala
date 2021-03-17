package mazes.models

import mazes.typeclasses.Show

final case class Grid(size: Int, cells: Vector[Vector[Cell]])

object Grid {

  // original grid is y by x flip will create x by y
  def flip(g: Grid): Grid = {
    g.cells.flatten.foldLeft(Grid.init(g.size)) {
      case (acc, elem) =>
        Grid(
          size = acc.size,
          cells = acc.cells.updated(
            elem.point.y,
            acc.cells(elem.point.y).updated(elem.point.x, elem)
          )
        )
    }
  }

  def init(size: Int): Grid = {
    val indices = 0.to(size - 1).toVector
    def cellWithNeighbors(size: Int, x: Int, y: Int): Cell =
      Cell.withUnlinkedNeighbors(
        point = Point(x, y),
        west = if (x == 0) { Option.empty[Point] }
        else { Option(Point(x - 1, y)) },
        south = if (y == 0) { Option.empty[Point] }
        else { Option(Point(x, y - 1)) },
        north = if (y == size - 1) { Option.empty[Point] }
        else { Option(Point(x, y + 1)) },
        east = if (x == size - 1) { Option.empty[Point] }
        else { Option(Point(x + 1, y)) }
      )
    Grid(
      size,
      indices.map(x => indices.map(y => cellWithNeighbors(size, x, y)))
    )
  }

  def get(g: Grid)(p: Point): Option[Cell] =
    g.cells.lift(p.x).flatMap(_.lift(p.y))

  def update(g: Grid)(c: Cell): Grid =
    Grid(
      g.size,
      g.cells.updated(c.point.x, g.cells(c.point.x).updated(c.point.y, c))
    )

  implicit val gridShow: Show[Grid] = (g: Grid) => {
    def topRow(c: Cell) =
      c match {
        case Cell(_, _, _, _, Some(e)) if e.hasLink => "    "
        case _                                      => "   |"
      }
    def bottom(c: Cell) =
      c match {
        case Cell(_, _, Some(s), _, _) if s.hasLink => "   "
        case _                                      => "---"
      }

    "+" + "---+".repeat(g.size) + "\n" +
      Grid
        .flip(g)
        .cells
        .reverse
        .map { r =>
          "|" + r.map(topRow).mkString + "\n" +
            "+" + r.map(bottom).mkString("+") + "+" + "\n"
        }
        .mkString("")
  }
}

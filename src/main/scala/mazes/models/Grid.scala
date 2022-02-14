package mazes.models

import mazes.typeclasses.Show

final case class Grid(
    rows: Int,
    columns: Int,
    cells: Vector[Vector[Cell]]
)

object Grid:

  def init(rows: Int, columns: Int): Grid = {
    val yI = 0.to(rows - 1).toVector
    val xI = 0.to(columns - 1).toVector
    def cellWithNeighbors(x: Int, y: Int): Cell =
      Cell.withUnlinkedNeighbors(
        point = Point(x, y),
        west = if (x == 0) { Option.empty[Point] }
        else { Option(Point(x - 1, y)) },
        south = if (y == 0) { Option.empty[Point] }
        else { Option(Point(x, y - 1)) },
        north = if (y == rows - 1) { Option.empty[Point] }
        else { Option(Point(x, y + 1)) },
        east = if (x == columns - 1) { Option.empty[Point] }
        else { Option(Point(x + 1, y)) }
      )
    Grid(
      rows = rows,
      columns = columns,
      cells = yI.map(y => xI.map(x => cellWithNeighbors(x, y)))
    )
  }

  def cellSet(g: Grid): Set[Cell] = g.cells.flatten.toSet

  def get(g: Grid)(p: Point): Option[Cell] =
    g.cells.lift(p.y).flatMap(_.lift(p.x))

  def update(g: Grid, c: Cell): Grid =
    Grid(
      rows = g.rows,
      columns = g.columns,
      cells =
        g.cells.updated(c.point.y, g.cells(c.point.y).updated(c.point.x, c))
    )

  given gridShow: Show[Grid] = (g: Grid) =>
    def topRow(c: Cell) =
      c match {
        case Cell(_, _, _, _, Some(e), isPath) if e.hasLink && isPath => "  x "
        case Cell(_, _, _, _, Some(e), isPath) if e.hasLink           => "    "
        case c if c.isPath                                            => "  x|"
        case _                                                        => "   |"
      }
    def bottom(c: Cell) =
      c match {
        case Cell(_, _, Some(s), _, _, _) if s.hasLink => "   "
        case _                                         => "---"
      }

    "+" + "---+".repeat(g.columns) + "\n" +
      g.cells.reverse
        .map { r =>
          "|" + r.map(topRow).mkString + "\n" +
            "+" + r.map(bottom).mkString("+") + "+" + "\n"
        }
        .mkString("")

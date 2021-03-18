package mazes.models

import mazes.typeclasses.Show

final case class Grid(
    rowLength: Int,
    columnLength: Int,
    cells: Vector[Vector[Cell]]
)

object Grid {

  // original grid is y by x flip will create x by y
  def flip(g: Grid): Grid = {
    g.cells.flatten.foldLeft(Grid.init(g.columnLength, g.rowLength)) {
      case (acc, elem) =>
        Grid(
          rowLength = acc.columnLength,
          columnLength = acc.rowLength,
          cells = acc.cells.updated(
            elem.point.y,
            acc.cells(elem.point.y).updated(elem.point.x, elem)
          )
        )
    }
  }

  def init(rowLength: Int, columnLength: Int): Grid = {
    val rI = 0.to(rowLength - 1).toVector
    val cI = 0.to(columnLength - 1).toVector
    def cellWithNeighbors(x: Int, y: Int): Cell =
      Cell.withUnlinkedNeighbors(
        point = Point(x, y),
        west = if (x == 0) { Option.empty[Point] }
        else { Option(Point(x - 1, y)) },
        south = if (y == 0) { Option.empty[Point] }
        else { Option(Point(x, y - 1)) },
        north = if (y == columnLength - 1) { Option.empty[Point] }
        else { Option(Point(x, y + 1)) },
        east = if (x == rowLength - 1) { Option.empty[Point] }
        else { Option(Point(x + 1, y)) }
      )
    Grid(
      rowLength = rowLength,
      columnLength = columnLength,
      cells = rI.map(x => cI.map(y => cellWithNeighbors(x, y)))
    )
  }

  def get(g: Grid)(p: Point): Option[Cell] =
    g.cells.lift(p.x).flatMap(_.lift(p.y))

  def update(g: Grid)(c: Cell): Grid =
    Grid(
      rowLength = g.rowLength,
      columnLength = g.columnLength,
      cells =
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

    "+" + "---+".repeat(g.rowLength) + "\n" +
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

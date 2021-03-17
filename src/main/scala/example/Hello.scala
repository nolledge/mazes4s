package example

import scala.util.Random

object Hello extends App {

  trait Show[T] {
    def show(t: T): String
  }

  def show[T](t: T)(implicit s: Show[T]): String = {
    s.show(t)
  }

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object East extends Direction
  case object West extends Direction

  final case class Point(
      x: Int,
      y: Int
  )

  final case class Cell(
      point: Point,
      west: Option[Neighbor],
      south: Option[Neighbor],
      north: Option[Neighbor],
      east: Option[Neighbor]
  )
  final case class Neighbor(
      point: Point,
      direction: Direction,
      hasLink: Boolean
  )

  object Cell {

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

  }
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
  // Binary tree
  def binaryTree(g: Grid): Grid = {
    val gridWithoutWalls = Grid(
      size = g.size,
      cells = g.cells.map(r => r.map(tearWall))
    )

    val withBidiLink = for {
      row <- gridWithoutWalls.cells
      cell <- row
      updatedCell <- setBidirectionalLink(gridWithoutWalls)(cell)
    } yield updatedCell

    withBidiLink.foldLeft(gridWithoutWalls) {
      case (acc, cell) => Grid.update(acc)(cell)
    }

  }
  // Fixme: implement direction
  def tearWall(c: Cell): Cell =
    c match {
      case Cell(_, _, _, None, None) => c
      case Cell(_, _, _, Some(_), None) =>
        Cell.linkCell(c, North)
      case Cell(_, _, _, None, Some(_)) =>
        Cell.linkCell(c, East)
      case Cell(_, _, _, Some(_), Some(_)) if Random.nextInt(2) == 0 =>
        Cell.linkCell(c, East)
      case Cell(_, _, _, Some(_), Some(_)) =>
        Cell.linkCell(c, North)
    }

  def setBidirectionalLink(g: Grid)(c: Cell): Option[Cell] =
    Cell
      .neighbors(c)
      .find(_.hasLink)
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

  val startGrid = Grid.init(20)
  val maze = binaryTree(startGrid)
  // println(maze)
  println(show(maze))
}

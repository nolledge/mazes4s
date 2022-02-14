package mazes.models

case class Distance(from: Cell, cost: Long)

object Dijkstra:

  def dijkstraDistances(root: Cell, maze: Grid): Map[Cell, Distance] =
    def go(
        curr: Cell,
        steps: Long,
        distances: List[(Cell, Distance)],
        toVisit: Set[Cell]
    ): List[(Cell, Distance)] =
      if (toVisit.isEmpty) distances
      else
        Cell
          .neighbors(curr)
          .filter(_.hasLink)
          .flatMap { n =>
            Grid
              .get(maze)(n.point)
              .filter(toVisit.contains)
              .fold(distances)(c =>
                go(
                  c,
                  steps + 1,
                  (distances :+ (c -> (Distance(curr, steps + 1L)))),
                  toVisit - curr
                )
              )

          }
          .toList

    go(
      root,
      0,
      List((root, Distance(root, 0))),
      Grid.cellSet(maze) - root
    )
      .groupBy(_._1)
      .mapValues(t => t.minBy(_._2.cost)._2)
      .toMap

  def shortestPath(
      root: Cell,
      to: Cell,
      dijkstraDistances: Map[Cell, Distance]
  ): List[Cell] =
    def go(currCell: Cell, cells: List[Cell]): List[Cell] = if (
      currCell == root
    ) {
      root +: cells
    } else {
      dijkstraDistances
        .get(currCell)
        .fold(cells)(c => go(c.from, (currCell +: cells)))
    }
    go(to, List.empty)

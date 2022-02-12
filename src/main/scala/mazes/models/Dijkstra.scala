package mazes.models

object Dijkstra:

  def fromRoot(root: Cell, maze: Grid): Map[Cell, Long] =
    def go(
        curr: Cell,
        steps: Long,
        distances: List[(Cell, Long)],
        toVisit: Set[Cell]
    ): List[(Cell, Long)] =
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
                  (distances :+ (c -> (steps + 1L))),
                  toVisit - curr
                )
              )

          }
          .toList

    go(root, 0, List((root, 0)), Grid.cellSet(maze) - root)
      .groupBy(_._1)
      .mapValues(t => t.minBy(_._2)._2)
      .toMap

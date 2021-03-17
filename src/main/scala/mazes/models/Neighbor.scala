package mazes.models

final case class Neighbor(
    point: Point,
    direction: Direction,
    hasLink: Boolean
)

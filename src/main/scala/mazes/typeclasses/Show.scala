package mazes.typeclasses

trait Show[T] {
  def show(t: T): String
}

object ShowOps {
  def show[T](t: T)(implicit s: Show[T]): String = {
    s.show(t)
  }
}

def flatten(xs: List[Any]): List[Any] = {
  xs match {
    case Nil => Nil
    case (x: List[Any]) :: ys => flatten(x) ::: flatten(ys)
    case x :: ys => x :: flatten(ys)
    }
  }
val l = List(1, List(2, List (3, 4, 5, 6)), List (3), 2)
flatten(l)

val a = (100, "l")

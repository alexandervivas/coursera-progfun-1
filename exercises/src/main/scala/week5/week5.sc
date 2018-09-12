object Week5 {

  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(_) => List()
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ++ List(y)
  }

  def removeAt[T](xs: List[T], n: Int): List[T] = n match {
    case 0 => xs.tail
    case _ => List(xs.head) ++ removeAt(xs.tail, n - 1)
  }

  def flatten(xs: List[Any]): List[Any] = {
    def innerFlatten(x: Any): List[Any] = x match {
      case y: List[Any] => flatten(y)
      case y => List(y)
    }
    xs match {
      case List() => throw new Error("flatten of empty list")
      case List(x) => innerFlatten(x)
      case y :: ys => innerFlatten(y) ++ flatten(ys)
    }
  }
}

Week5.removeAt(List('a', 'b', 'c', 'd'), 1)
Week5.flatten(List(List(1, 1), 2, List(3, List(5, 8))))
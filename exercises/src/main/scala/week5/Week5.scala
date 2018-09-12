package week5

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

  def removeAtV1[T](xs: List[T], n: Int): List[T] = n match {
    case 0 => xs.tail
    case _ => List(xs.head) ++ removeAtV1(xs.tail, n - 1)
  }

  def removeAtV2[T](xs: List[T], n: Int): List[T] = (xs take n) ::: (xs drop n + 1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => throw new Error("flatten of empty list")
    case y :: ys => List(y match {
      case List(_) => flatten(_)
      case _ => _
    }) ++ flatten(ys)
  }
}
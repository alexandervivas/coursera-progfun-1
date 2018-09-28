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

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if(n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (_, Nil) || (Nil, _) => _
        case (x :: xs1, y :: ys1) =>
          if(x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }
}
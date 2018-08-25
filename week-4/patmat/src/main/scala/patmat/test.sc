val chars = List('a', 'b', 'a')

def times(chars: List[Char]): List[(Char, Int)] = chars match {
  case Nil => Nil
  case head :: tail =>
    times(tail).foldLeft(List[(Char, Int)]()) { (accumulated, current) =>
      current match {
        case (c, i) => {
          println(s"($c, $i)")
          accumulated ::: (if(head == c) List((c, i + 1)) else List(current))
        }
      }
    }
}

times(chars)
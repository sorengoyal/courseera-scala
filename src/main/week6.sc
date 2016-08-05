def isPrime(n: Int): Boolean = {
  (2 to n-1).map(n%_).forall(_!=0)
}

isPrime(97)
isPrime(100)

def decomposePrime(n: Int): List[(Int, Int)] = {
  (1 until n).
    flatMap(i => (1 until i).map(j => (i, j))).
    toList.
    filter({ case (x, y) => isPrime(x + y) })
}

decomposePrime(7).foreach(println(_))

def scalarProduct(xs: List[Double], ys: List[Double]) = {
  (for((x,y) <- xs zip ys) yield x*y).sum
}

scalarProduct(List(1,2,3), List(1,1,1))

val as = (1 -> 10)
case class Poly(terms:Map[Int, Double]) {
  def this(bindings:(Int,Double)*) = this(bindings.toMap)
  def + (other: Poly) = {
    new Poly(other.terms.foldLeft(terms)(addTerm))
  }
  def addTerm(map1: Map[Int, Double], term:(Int, Double)): Map[Int, Double] = {
    term match {
      case (e, c) => {
        map1.get(e) match {
          case None => map1 + term
          case Some(value: Double) => map1.+((e, c + value))
        }
      }
    }
  }
  override def toString =
    (for((e,c) <- terms) yield (c + "x" + e)).mkString("+")
}
val p1 = new Poly(2 -> 1, 1 -> 1,  0-> 1)
val p2 = new Poly(2 -> 2, 1 -> 1)
val p3 = p1 + p2

import scala.io.Source

object x {
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords")
  val words = in.getLines.toList.filter(!_.contains('-'))
  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )
  val charCode: Map[Char, Char] = mnem.flatMap({ case(n, str) => (for(c <- str) yield (c,n)).toMap})
  def wordCode(word: String) = word.toUpperCase.map(charCode(_))

}


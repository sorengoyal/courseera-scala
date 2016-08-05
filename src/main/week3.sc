//Twice(Int): Int, number(Int=>Int): Int
class Number(n: Int) {
  def op(d: Twice): Int = {
    d.op(n)
  }
}

class Twice {
  def op(n: Int): Int = 2*n
}

val a = new Number(10)
val t = new Twice
a.op(t)

def Person(height: Int, weight: Int): () => Int = {
  () => {weight/height/height}
}
val p = Person(2, 100)
p()

def pickFromList[T](n: Int, list: List[T]): T = {
  if(n > list.length-1 || n < 0)
    throw new IndexOutOfBoundsException
  else
    list(n)
}
val list:List[Int] = 1::2::3::4::5::6::Nil
pickFromList(1,list)
pickFromList(-1,list)

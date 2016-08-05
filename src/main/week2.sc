type Set = Int => Boolean

def singletonSet(n: Int) = {
  (x: Int) => x == n
}

def union(a: Set, b: Set) = {
  (x: Int) => a(x) || b(x)
}

def intersect(a: Set, b: Set) = {
  (x: Int) => a(x) && b(x)
}

def diff(a: Set, b: Set) = {
  (x: Int) => a(x) && !b(x)
}
def filter(a: Set, p: (Int) => Boolean) = {
  (x: Int) => a(x) && p(x)
}

def forall(s: Set, p: Int => Boolean): Boolean = {
  def checkFrom(n: Int): Boolean = {
    if(n == 1001)
      true
    else if(s(n) && !p(n))
      false
    else
      checkFrom(n+1)
  }

  checkFrom(-1000)
}

def exists(s: Set, p: Int => Boolean) = {
  !forall(s, !p(_))// == false )
}

def map(s: Set, f: Int => Int): Set = {
  (x: Int) => {
    exists(s, f(_) == x)
  }
}
val a = singletonSet(10)
val b = singletonSet(20)
val c = singletonSet(23)
val cent = {(x: Int) => x >= 0 && x <=100}
val ab = union(a, b)
val abc = union(ab, c)
val p = (x: Int) => {x == 10 || x == 20}

forall(ab, p)
exists(abc, _%10 == 3)

val abc1 = map(cent, 2*_)
abc1(200)

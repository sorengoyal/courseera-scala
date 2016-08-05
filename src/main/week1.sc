def squareRoot(estimate: Double, n: Double): Double = {
  print(estimate)
  if(guessIsGoodEnough(estimate, n))
    estimate
  else
    squareRoot(improve(estimate, n), n)
}

def guessIsGoodEnough(estimate: Double, n: Double) = {
  estimate*estimate/n < 1.001 && estimate*estimate/n > 0.999
}

def improve(estimate: Double, n: Double): Double =
  (estimate + n/estimate)/2

val a = squareRoot(1, 1e-20)

//Tail recursive version of Factorial

def fact(x: Int, y:Int): Int = {
  if(x == 1)
    y
  else
    fact(x-1, x*y)
}

val f = fact(5, 1)




abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def toString = "0"
  def isZero = true
  def predecessor = throw new NoSuchElementException
  def successor = new Succ(this)
  def + (that: Nat) = that
  def - (that: Nat) = if(that.isZero) this else throw new IllegalArgumentException
}
class Succ(n: Nat) extends Nat {
  override def toString = n.toString + "+1"
  def isZero = false
  def predecessor = n
  def successor = new Succ(this)
  def + (that: Nat) = {
    new Succ(n + that)
  }
  def - (that: Nat) = {
    if(that.isZero)
      this
    else
      n - that.predecessor
  }
}

val one = Zero.successor
val two = one + one
val three = two + one
val two2 = three - one

val zero2 = three - three




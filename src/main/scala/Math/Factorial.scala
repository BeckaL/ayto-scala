package Math

object Factorial {

  def factorial(n: Int): Int = n match {
    case 1 => 1
    case x => x * factorial(x - 1)
  }
}

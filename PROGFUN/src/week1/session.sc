package week1

object session {
  1 + 2                                           //> res0: Int(3) = 3
  //println("Welcome to the Scala worksheet")
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

   def isGoodEnough(guess: Double) =
     abs(guess * guess - x) / x < 0.00001

    def improve(guess: Double) =
     (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  sqrt(2)                                         //> res1: Double = 1.4142156862745097

  sqrt(9)                                         //> res2: Double = 3.000000001396984

  sqrt(1e-6)                                      //> res3: Double = 0.0010000001533016628

  sqrt(1e6)                                       //> res4: Double = 1000.0001533016629
}
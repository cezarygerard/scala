package week3

import math.abs

object session {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(73); 

	val tolerance = 0.0001;System.out.println("""tolerance  : Double = """ + $show(tolerance ));$skip(80); 
	
	def isCloseEnough(x: Double, y: Double) =
		abs((x - y) / x) / x < tolerance;System.out.println("""isCloseEnough: (x: Double, y: Double)Boolean""");$skip(219); 
	
	def fixedPoint(f: Double => Double)(firstGuess: Double) = {
		def iterate(guess: Double): Double = {
			val next = f(guess)
			if (isCloseEnough(guess, next)) next
			else iterate(next)
		}
	
	iterate(firstGuess)
 };System.out.println("""fixedPoint: (f: Double => Double)(firstGuess: Double)Double""");$skip(27); val res$0 = 
 fixedPoint(x => 1+x/2)(1);System.out.println("""res0: Double = """ + $show(res$0));$skip(48); 
 def sqrt(x: Double) = fixedPoint(y => x/y )(1);System.out.println("""sqrt: (x: Double)Double""")}
 
}

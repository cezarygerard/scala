package week2

object session {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(160); 
	def factorial(n:Int) : Int ={
		def loop (acc: Int, n: Int) : Int  =
		if(n ==0)acc
		else loop(acc*n , n-1)
		
		loop (1,n)
	};System.out.println("""factorial: (n: Int)Int""");$skip(17); val res$0 = 
	
	factorial (9);System.out.println("""res0: Int = """ + $show(res$0));$skip(144); 
	
		def sum(f: Int => Int): (Int, Int) => Int = {
		def sumF(a: Int, b: Int): Int =
			if (a > b) 0
			else f(a) + sumF(a + 1, b)
			
		sumF
	};System.out.println("""sum: (f: Int => Int)(Int, Int) => Int""");$skip(29); 
	
	def sumInts = sum(x => x);System.out.println("""sumInts: => (Int, Int) => Int""");$skip(36); 
	def sumCubes = sum(x => x * x * x);System.out.println("""sumCubes: => (Int, Int) => Int""");$skip(18); val res$1 = 
	
	sumInts(5, 10);System.out.println("""res1: Int = """ + $show(res$1))}




}

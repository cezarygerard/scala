package week2

object session {
	def factorial(n:Int) : Int ={
		def loop (acc: Int, n: Int) : Int  =
		if(n ==0)acc
		else loop(acc*n , n-1)
		
		loop (1,n)
	}                                         //> factorial: (n: Int)Int
	
	factorial (9)                             //> res0: Int = 362880
	
		def sum(f: Int => Int): (Int, Int) => Int = {
		def sumF(a: Int, b: Int): Int =
			if (a > b) 0
			else f(a) + sumF(a + 1, b)
			
		sumF
	}                                         //> sum: (f: Int => Int)(Int, Int) => Int
	
	def sumInts = sum(x => x)                 //> sumInts: => (Int, Int) => Int
	def sumCubes = sum(x => x * x * x)        //> sumCubes: => (Int, Int) => Int
	
	sumInts(5, 10)                            //> res1: Int = 45




}
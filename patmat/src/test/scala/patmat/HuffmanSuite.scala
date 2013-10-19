package patmat

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._
import org.junit.Ignore

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    def a = 'a'
    def b = 'b'
    def c = 'c'
    def d = 'd'
    def e = 'e'
    def f = 'f'
    def h = 'g'
    def i = 'h'
    def g = 'i'
    def j = 'j'
    def k = 'k'
    def l = 'l'
    def m = 'm'
    def n = 'n'
    def o = 'o'
    def p = 'p'
    def r = 'r'
    def q = 'q'
    def s = 's'
    def t = 't'
    def u = 'u'
    def v = 'v'
    def w = 'w'
    def x = 'x'
    def y = 'y'
    def z = 'z'

	val t3 = Fork(Fork(Fork(Fork(Fork(Leaf(y,3),Leaf(u,3),List(y, u),6),Leaf(x,6),List(y, u, x),12),Fork(Leaf(d,7),Leaf(k,7),List(d, k),14),List(y, u, x, d, k),26),Fork(Fork(Fork(Leaf(z,4),Leaf(l,4),List(z, l),8),Fork(Fork(Leaf(q,2),Leaf(i,2),List(q, i),4),Fork(Leaf(v,2),Leaf(r,2),List(v, r),4),List(q, i, v, r),8),List(z, l, q, i, v, r),16),Fork(Fork(Fork(Fork(Leaf(e,1),Leaf(w,1),List(e, w),2),Leaf(b,2),List(e, w, b),4),Leaf(h,4),List(e, w, b, h),8),Fork(Leaf(j,4),Fork(Leaf(g,2),Leaf(o,3),List(g, o),5),List(j, g, o),9),List(e, w, b, h, j, g, o),17),List(z, l, q, i, v, r, e, w, b, h, j, g, o),33),List(y, u, x, d, k, z, l, q, i, v, r, e, w, b, h, j, g, o),59),Fork(Fork(Fork(Leaf(s,9),Leaf(c,9),List(s, c),18),Fork(Fork(Leaf(n,5),Leaf(f,5),List(n, f),10),Fork(Fork(Leaf(p,3),Leaf(t,3),List(p, t),6),Leaf(m,6),List(p, t, m),12),List(n, f, p, t, m),22),List(s, c, n, f, p, t, m),40),Leaf(a,59),List(s, c, n, f, p, t, m, a),99),List(y, u, x, d, k, z, l, q, i, v, r, e, w, b, h, j, g, o, s, c, n, f, p, t, m, a),158)
 val t4= Fork(Fork(Leaf(c,3),Fork(Leaf(a,1),Leaf(b,2),List(a, b),3),List(c, a, b),6),Fork(Leaf(d,4),Leaf(e,5),List(d, e),9),List(c, a, b, d, e),15)

  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("sortowanie") {
    assert(isort(string2Chars("dupa"), charComp) === List('a', 'd', 'p', 'u'))
  }

  test("sortowanie2") {
    assert(times(isort(string2Chars("dupaaaarru"), charComp)) === List(('a', 4), ('d', 1), ('p', 1), ('r', 2), ('u', 2)))
  }

  test("liczenie") {
    assert(times(isort(string2Chars("dupaaaarru"), charComp)) === List(('a', 4), ('d', 1), ('p', 1), ('r', 2), ('u', 2)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))

  }

  test("singleton") {
    assert(singleton(List(Leaf('e', 1))) === true)
  }

  test("until") {
    val pairs = times(isort(string2Chars("dupaaaarru"), charComp))
    val ordered = makeOrderedLeafList(pairs)
    val unrilRes = until(singleton, combine)(ordered)
    // println(unrilRes)
  }

  test("createCodeTree") {
    println(createCodeTree(string2Chars("AAAABCDEEEFFGH")))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("french secret") {
    println(decodedSecret)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  //    test("codeBits"){
  //      val a = List(0,0)
  //      val b = List(0,1)
  //      val c = List(1,0)
  //      val d = List(1,1)
  //      val table = List(('a',a),('b',b),('c',c),('d',d))
  //      
  //    println(codeBits(table)('a'))
  //    println(codeBits(table)('b'))
  //    println(codeBits(table)('c'))
  //    println(codeBits(table)('d'))
  //  }

  //	test("codeBits - 2"){
  //		val a = List(0)
  //				//      val b = List(0,1)
  //				val c = List(1,0)
  //				val d = List(1,1)
  //				val table = List(('a',a),('c',c),('d',d))
  //				println(table)
  //				println(codeBits(table)('a'))
  //				//    println(codeBits(table)('b'))
  //				println(codeBits(table)('c'))
  //				println(codeBits(table)('d'))
  //	}
  //
  test("mergeCodeTables") {
    println("mergeCodeTables")
    val a = List(0)
    val b = List(1)
    val c = List(0)
    val d = List(1)
    val table1 = List(('a', a), ('b', b))
    val table2 = List(('c', c), ('d', d))
    val table = mergeCodeTables(table1, table2)

    println(table1)
    println(table2)
    println(table)

  }

  test("mergeCodeTables2") {
    println("mergeCodeTables2")
    val a = List(0)
    //   val b = List(1)
    // val c = List(0)
    val d = List(1)
    val table1 = List(('a', a))
    val table2 = List(('d', d))
    val table = mergeCodeTables(table1, table2)

    println(table1)
    println(table2)
    println(table)

  }
  test("convert") {
    println("convert")
    val table = convert(createCodeTree(string2Chars("AAAAAAABCDEEEFFGH")))
    println(table)
  }

  test("convert 2") {
    println("convert 2")
    new TestTrees {

      val table = convert(t1)
      println(table)
      val table2 = convert(t2)
      println(table2)
    }
  }
//  ////	     
//  test("decode and encode a very short text should be identity [2]") {
//    println("============ OSTATNI: ")
//    new TestTrees {
//      val a = encode(t1)("abba".toList);
//      val b = quickEncode(t1)("abba".toList)
//      println(a)
//      println(b)
//      println(decode(t1, a))
//      println(decode(t1, b))
//    }
//  }
  //	    
  test("quickencode 1") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quickencode 2") {
    new TestTrees {
      assert(quickEncode(t2)("ababbddaaadbbdaaaaabbd".toList) === encode(t2)("ababbddaaadbbdaaaaabbd".toList))
    }
  }

  test("quickencode 3") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("abdabbd".toList)) === "abdabbd".toList)
    }
  }

  test("quickencode 4") {
    new TestTrees {
      assert(decode(t2, (quickEncode(t2)("ababbddaaadbbdaaaaabbd".toList)))
        === decode(t2, encode(t2)("ababbddaaadbbdaaaaabbd".toList)))
    }
  }

  test(" quickencode 5") {
    new TestTrees {
      println(createCodeTree(string2Chars("haxfhkfxaxfajmgnkcdzlmqpoaimsxdsxcotuzdaaaackscmsjhyspmbtyfrvaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaczcnldskjcnsdlknckdsuqwertyuiopasdfghjklzxcvbnm")))
    }
  }

   test(" creat tree") {
    new TestTrees {
      println(" creat tree")
      println(createCodeTree(string2Chars("abbcccddddeeeee")))
    }
  }
    test("quickencode 6") {
    new TestTrees {
	      assert(decode(t3, (quickEncode(t3)("dupabiskupa".toList)))
	        === decode(t3, encode(t3)("dupabiskupa".toList)))
	    }
    }
    
     test("quickencode 7") {
    new TestTrees {
	      assert(decode(t3, (quickEncode(t3)("dupabiskupa".toList)))
	        === decode(t3, quickEncode(t3)("dupabiskupa".toList)))
	    }
    }
     
    test("quickencode 8") {
    new TestTrees {
	      assert(decode(t3, (quickEncode(t3)("d".toList)))
	        === decode(t3, encode(t3)("d".toList)))
	    }
    }
    
//         
//    test("quickencode 9") {
//    new TestTrees {
////	      assert(decode(t3, (quickEncode(t3)("d".toList)))
////	        === decode(t3, encode(t3)("d".toList)))
//    	println(encode(t2)("d".toList))
//    	println(encode(t2)("b".toList))
//    	println(encode(t2)("a".toList))
//    	println(quickEncode(t2)("d".toList))
//    	println(quickEncode(t2)("b".toList))
//    	println(quickEncode(t2)("a".toList))
//      
//    	}
//    }
    
        test("quickencode 10") {
    new TestTrees {
//	      assert(decode(t3, (quickEncode(t3)("d".toList)))
//	        === decode(t3, encode(t3)("d".toList)))
    	println(encode(t4)("a".toList))
    	println(encode(t4)("b".toList))
    	println(encode(t4)("c".toList))
    	println(encode(t4)("d".toList))
    	println(encode(t4)("e".toList))
    	println(quickEncode(t4)("a".toList))
    	println(quickEncode(t4)("b".toList))
    	println(quickEncode(t4)("c".toList))
    	println(quickEncode(t4)("d".toList))
    	println(quickEncode(t4)("e".toList))
      
    	}
    }
    
}
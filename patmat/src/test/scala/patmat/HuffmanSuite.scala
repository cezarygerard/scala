package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("sortowanie"){
    assert(isort(string2Chars("dupa"), charComp) === List('a','d','p','u'))
  }
  
   test("sortowanie2"){
    assert(times(isort(string2Chars("dupaaaarru"), charComp)) === List(('a',4),('d',1),('p',1),('r',2),('u',2)))
  }
  
   test("liczenie"){
    assert(times(isort(string2Chars("dupaaaarru"), charComp)) === List(('a',4),('d',1),('p',1),('r',2),('u',2)))
  }
  
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))

  }
  
  test("singleton" ) {
    assert(singleton(List(Leaf('e',1))) === true)
  }
  
  test("until"){
    val pairs = times(isort(string2Chars("dupaaaarru"), charComp))
    val ordered = makeOrderedLeafList(pairs)
    val unrilRes = until(singleton, combine)(ordered)
   // println(unrilRes)
  }
  
  test("createCodeTree"){
   // println (createCodeTree(string2Chars("AAAABCDEEEFFGH") ))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("french secret"){
    println(decodedSecret)
  }
  

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
 }
}
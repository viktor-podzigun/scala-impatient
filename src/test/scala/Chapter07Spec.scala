import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class Chapter07Spec extends FlatSpec with Matchers {

  "puzzler" should "use a package com that isn’t at the top level" in {
    com.FromCom.value shouldBe 1

    import puzzler._
    com.FromCom.value shouldBe 21
  }

  "random" should "has nextInt, nextDouble, and setSeed functions" in {
    random.nextInt() shouldBe 1013904223
    random.nextDouble() shouldBe (((1013904223 * 1664525) + 1013904223)/(Int.MaxValue + 1.0))

    random.setSeed(1)
    random.nextInt() shouldBe 1015568748
  }

  "Chapter0706" should "copy all elements from a Java hash map into a Scala hash map" in {
    //given
    val javaHashMap = new java.util.HashMap[String, Int]
    javaHashMap.put("A", 1)
    javaHashMap.put("B", 2)
    javaHashMap.put("C", 3)

    //when
    val scalaHashMap: mutable.HashMap[String, Int] = Chapter0706.fromJavaHashMap(javaHashMap)

    //then
    scalaHashMap.size shouldBe javaHashMap.size
    scalaHashMap("A") shouldBe 1
    scalaHashMap("B") shouldBe 2
    scalaHashMap("C") shouldBe 3
  }
}

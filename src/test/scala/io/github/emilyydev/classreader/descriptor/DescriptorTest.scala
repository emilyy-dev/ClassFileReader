package io.github.emilyydev.classreader.descriptor

import org.scalatest.flatspec._
import org.scalatest.matchers._

class DescriptorTest extends AnyFlatSpec with should.Matchers {
  it should "handle single-character descriptor" in {
    DescriptorReader.convertField("I") should be(("int", 1))
    DescriptorReader.convertField("C") should be(("char", 1))
  }

  it should "handle array of simple types" in {
    DescriptorReader.convertField("[Z") should be(("boolean[]", 2))
  }

  it should "handle array of arrays" in {
    DescriptorReader.convertField("[[[J") should be(("long[][][]", 4))
  }

  it should "handle reference types" in {
    DescriptorReader.convertField("Ljava/lang/Object;") should be(("java.lang.Object", 18))
  }

  it should "reject invalid characters" in {
    a[NotImplementedError] should be thrownBy {
      DescriptorReader.convertField("X")
    }
  }

  it should "handle method types" in {
    DescriptorReader.method("(IDLjava/lang/Thread;)Ljava/lang/Object;") should be(
      "java.lang.Object (int, double, java.lang.Thread)"
    )
    DescriptorReader.method("()V") should be("void ()")
  }
}

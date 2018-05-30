// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.annotations.{ModuleName, CircuitName}
import firrtl.transforms.{SplitInstanceAnnotation, SplitInstanceException}

class SplitInstanceSpec extends FirrtlFlatSpec {
  behavior of "SplitInstanceAnnotation"

  it should "error if instance and module do not match" in {
    val m = ModuleName("Foo", CircuitName("Top"))
    val p = Seq(Seq(WDefInstance("Top", "Top"), WDefInstance("Foo", "Bar")))
    a [SplitInstanceException] should be thrownBy SplitInstanceAnnotation(m, p)
  }
  it should "error if the top module is specified to be split" in {
    val m = ModuleName("Top", CircuitName("Top"))
    val p = Seq(Seq(WDefInstance("Top", "Top")), Seq(WDefInstance("Top", "Top"), WDefInstance("foo", "Top")))
    a [SplitInstanceException] should be thrownBy SplitInstanceAnnotation(m, p)
  }
  it should "error if paths do not share the same top module" in {
    val m = ModuleName("Foo", CircuitName("Top"))
    val p = Seq(Seq(WDefInstance("Top", "Top"), WDefInstance("foo", "Foo")),
                Seq(WDefInstance("OtherTop", "OtherTop"), WDefInstance("foo", "Foo")))
    a [SplitInstanceException] should be thrownBy SplitInstanceAnnotation(m, p)
  }

  behavior of "SplitInstance Transform"
}

package project1.public.bitmanipulation.clz

import bitmanipulation.LeadingZerosCounter
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LeadingZerosCounterTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "LeadingZerosCounter"

for (i <- 1 until 6) {
  val bitWidth = math.pow(2, i).toInt
  s"LeadingZerosCounter($bitWidth)" should "count number of leading bits correctly" in {
    test(new LeadingZerosCounter(bitWidth)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c =>
        c.io.input.poke(0.U)
        c.io.result.expect(bitWidth.U)

        c.io.input.poke(1.U)
        c.io.result.expect((bitWidth-1).U)

        c.io.input.poke(2.U)
        c.io.result.expect((bitWidth-2).U)

//        c.io.input.poke(math.pow(2, bitWidth-1).toInt.U)
//        c.io.result.expect(0.U)
    }
  }
}
/* 
  "LeadingZerosCounter(2)" should "count number of leading bits correctly" in {
    test(new LeadingZerosCounter(2)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c =>
        c.io.input.poke(0.U)
        c.io.result.expect(2.U)

        c.io.input.poke(1.U)
        c.io.result.expect(1.U)
    }
  }

  "LeadingZerosCounter(4)" should "count number of leading bits correctly" in {
    test(new LeadingZerosCounter(4)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c =>
        c.io.input.poke(0.U)
        c.io.result.expect(4.U)

        c.io.input.poke(4.U)
        c.io.result.expect(1.U)
    }
  }

  "LeadingZerosCounter(32)" should "count number of leading bits correctly" in {
    test(new LeadingZerosCounter(32)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c =>
        c.io.input.poke(0.U)
        c.io.result.expect(32.U)

        c.io.input.poke(1.U)
        c.io.result.expect(31.U)
    }
  } */
}

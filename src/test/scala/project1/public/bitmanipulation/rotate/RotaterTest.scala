package project1.public.bitmanipulation.rotate

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import bitmanipulation._

class RotateBitTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "Rotater"

  "SequentialRotater" should "rotate right by 0" in {
    test(new SequentialRotater(32, () => new FixedRotater(32, 1)))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.io.input.poke(0.U(32.W))
        c.io.shamt.poke(0.U(5.W))
        c.io.start.poke(true.B)
        c.io.result.expect(0.U(32.W))
        c.io.done.expect(true.B)
      }
  }

  "SequentialRotater" should "rotate right by 1" in {
    test(new SequentialRotater(32, () => new FixedRotater(32, 1)))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.io.input.poke(8.U(32.W))
        c.io.shamt.poke(1.U(5.W))
        c.io.start.poke(true.B)
        c.clock.step(1)
        c.io.result.expect(4.U(32.W))
        c.io.done.expect(true.B)
      }
  }
  "SequentialRotater" should "rotate right by 3" in {
    test(new SequentialRotater(32, () => new FixedRotater(32, 1)))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.io.input.poke(8.U(32.W))
        c.io.shamt.poke(3.U(5.W))
        c.io.start.poke(true.B)
        c.clock.step(1)
        c.io.start.poke(false.B)
        c.clock.step(2)
        c.io.result.expect(1.U(32.W))
        c.io.done.expect(true.B)
      }
  }

  "SequentialRotater" should "rotate right by 5" in {
    test(new SequentialRotater(32, () => new FixedRotater(32, 1)))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.io.input.poke(8.U(32.W))
        c.io.shamt.poke(5.U(5.W))
        c.io.start.poke(true.B)
        c.clock.step(1)
        c.io.start.poke(false.B)
        c.clock.step(4)
        c.io.result.expect(BigInt("40000000", 16).U(32.W))
        c.io.done.expect(true.B)
      }
  }

  "FixedRotater" should "rotate right by 3" in {
    test(new FixedRotater(32, 3))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.io.input.poke(8.U(32.W))
        c.io.result.expect(1.U(32.W))
      }
  }

  "FixedRotater" should "rotate right by 6" in {
    test(new FixedRotater(32, 6))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.io.input.poke(8.U(32.W))
        c.io.result.expect(BigInt("20000000", 16).U(32.W))
      }
  }

}

package project1.public.bitmanipulation.shuffle

import bitmanipulation.Shuffler
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShufflerTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "Shuffler"

  it should "do nothing" in {
    test(new Shuffler(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("AAAAAAAA", 16).U(32.W)
      val unshuffle = 0.U(1.W)
      val pattern = 0.U(5.W)
      val expected = input

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }

  it should "do shuffle" in {
    test(new Shuffler(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("0000FFFF", 16).U(32.W)
      val unshuffle = 0.U(1.W)
      val pattern = 0b1111.U(4.W)
      val expected = BigInt("55555555", 16).U(32.W)

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)

      c.io.result.expect(expected)
    }
  }

  it should "do unshuffle" in {
    test(new Shuffler(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("AAAAAAAA", 16).U(32.W)
      val unshuffle = 1.U(1.W)
      val pattern = 0b1111.U(4.W)
      val expected = BigInt("FFFF0000", 16).U(32.W)

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }

  it should "swap bits at indices 1 and 2" in {
    test(new Shuffler(4)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("A", 16).U(4.W)
      val unshuffle = 0.U(1.W)
      val pattern = 1.U(1.W)
      val expected = BigInt("C", 16).U(4.W)

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }
  it should "swap bits at indices 1 and 2, 2 and 4, 3 and 5 and so on" in {
    test(new Shuffler(8)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("F", 16).U(8.W)
      val unshuffle = 0.U(1.W)
      val pattern = 0b11.U(2.W)
      val expected = BigInt("55", 16).U(8.W)

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }

  it should "swap bits at indices 1 and 2, 5 and 6, 9 and 10, etc." in {
    test(new Shuffler(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("AAAAAAAA", 16).U(32.W)
      val unshuffle = 0.U(1.W)
      val pattern = 1.U(4.W)
      val expected = BigInt("CCCCCCCC", 16).U(32.W)

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }

  it should "do shuffe with mode[1] followed by mode[0]" in {
    test(new Shuffler(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("2C3C07F1", 16).U(32.W)
      val unshuffle = 0.U(1.W)
      val pattern = 0b0011.U(4.W)
      val expected = BigInt("585A15AB", 16).U(32.W)

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io. unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }

  it should "do unshuffe with mode[0] followed by mode[1]" in {
    test(new Shuffler(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("04789243", 16).U(32.W)
      val unshuffle = 1.U(1.W)
      val pattern = 0b0011.U(4.W)
      val expected = BigInt("026C9419", 16).U(32.W)

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io. unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }

  it should "do shuffe with mode[0] followed by mode[1]" in {
    test(new Shuffler(64)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("2C3C07F100000000", 16).U(64.W)
      val unshuffle = 0.U(1.W)
      val pattern = 0b00011.U(4.W)
      val expected = BigInt("585A15AB00000000", 16).U(64.W)

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io. unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }
}
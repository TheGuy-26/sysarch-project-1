package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractLeadingZerosCounter(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val result = Output(UInt(log2Ceil(bitWidth + 1).W))
  })
}

// You may expect bitWidth to be a power of two.
class LeadingZerosCounter(bitWidth: Int)
    extends AbstractLeadingZerosCounter(bitWidth) {
  if (bitWidth == 1)
    io.result := !(io.input(0).asBool)
  else {
    val left = Module(new LeadingZerosCounter(bitWidth / 2))
    val right = Module(new LeadingZerosCounter(bitWidth / 2))

    left.io.input := io.input(bitWidth - 1, bitWidth / 2)
    right.io.input := io.input(bitWidth / 2 - 1, 0)
    when(left.io.result === (bitWidth / 2).U && right.io.result > 0.U) {
      io.result := Mux(right.io.result === (bitWidth / 2).U,
        bitWidth.U,
        1.U(1.W) ## right.io.result(log2Ceil(bitWidth) - 2, 0))
    }.otherwise {
      io.result := left.io.result
    }
  }
}
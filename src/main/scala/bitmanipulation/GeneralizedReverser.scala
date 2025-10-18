package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractGeneralizedReverser(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val pattern = Input(UInt(log2Ceil(bitWidth).W))
    val result = Output(UInt(bitWidth.W))
  })
}

class GeneralizedReverser(bitWidth: Int)
    extends AbstractGeneralizedReverser(bitWidth) {
  if (bitWidth == 1) {
    io.result := io.input
  } else {
    val d = log2Ceil(bitWidth)

    val left = Module(new GeneralizedReverser(bitWidth / 2))
    val right = Module(new GeneralizedReverser(bitWidth / 2))

    left.io.pattern := io.pattern(d - 2, 0)
    right.io.pattern := io.pattern(d - 2, 0)

    when(io.pattern(d-1) === 1.U) {
      left.io.input := io.input(bitWidth / 2 - 1, 0)
      right.io.input := io.input(bitWidth - 1, bitWidth / 2)
      io.result := Cat(left.io.result, right.io.result)
    }.otherwise {
      right.io.input := io.input(bitWidth / 2 - 1, 0)
      left.io.input := io.input(bitWidth - 1, bitWidth / 2)
      io.result := Cat(left.io.result, right.io.result)
    }
  }
}

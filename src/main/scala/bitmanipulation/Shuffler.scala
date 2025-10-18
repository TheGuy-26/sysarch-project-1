package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractShuffler(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val pattern = Input(UInt((log2Ceil(bitWidth) - 1).W))
    val unshuffle = Input(UInt(1.W))
    val result = Output(UInt(bitWidth.W))
  })
}

class Shuffler(bitWidth: Int) extends AbstractShuffler(bitWidth) {

  val patternWidth = log2Ceil(bitWidth) - 1

  val result = Mux(
    (io.unshuffle === 0.U),
    (0 until patternWidth).reverse.foldLeft(io.input) {
      (current_result, stage) =>
        Mux(
          io.pattern(stage),
          shuffleStage(current_result, (1 << stage)),
          current_result
        )
    },
    (0 until patternWidth).foldLeft(io.input) { (current_result, stage) =>
      Mux(
        io.pattern(stage),
        shuffleStage(current_result, (1 << stage)),
        current_result
      )
    }
  )

  io.result := result

  def shuffleStage(data: UInt, N: Int): UInt = {
    val (maskL, maskR) = getMasks(N)
    val left = (data << N) & maskL
    val right = (data >> N) & maskR
    val preserve = data & ~(maskL | maskR)
    preserve | left | right
  }

 def getMasks(N: Int): (UInt, UInt) = {
   val maskL =
     VecInit(VecInit.tabulate(bitWidth) { i =>
      if (i < N) false.B else (((i - N) % (N * 4)) < N).B
    }.reverse).asUInt

   val maskR = maskL >> N.U

   (maskL, maskR)
 }
}

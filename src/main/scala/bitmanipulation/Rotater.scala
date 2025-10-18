package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractFixedRotater(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val result = Output(UInt(bitWidth.W))
  })
}

class FixedRotater(bitWidth: Int, shamt: Int)
    extends AbstractFixedRotater(bitWidth) {

  val actualShamt = shamt.U & (bitWidth - 1).U
  val rot = (io.input >> actualShamt) | (io.input << ((bitWidth.U - actualShamt) & (bitWidth - 1).U))

  io.result := rot
}

abstract class AbstractSequentialRotater(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val shamt = Input(UInt(log2Ceil(bitWidth).W))
    val start = Input(Bool())
    val done = Output(Bool())
    val result = Output(UInt(bitWidth.W))
  })
}

class SequentialRotater(bitWidth: Int, generator: () => AbstractFixedRotater)
    extends AbstractSequentialRotater(bitWidth) {
      
  val Rotater = Module(generator())

  // create registers to hold state
  val counter = RegInit(0.U(log2Ceil(bitWidth).W))
  val intermediateRot = RegInit(0.U(bitWidth.W))
  val done = RegInit(false.B)
  val started = RegInit(false.B)

  // connections
  Rotater.io.input := Mux(started, intermediateRot, io.input)
  io.done := done || (io.shamt === 0.U)
  io.result := Rotater.io.result

  when(io.start && !started) {
    started := true.B
    counter := io.shamt - 1.U
    intermediateRot := io.input
    done := (io.shamt === 1.U)
  }.elsewhen(started && !done) {
    intermediateRot := Rotater.io.result
    counter := counter - 1.U
    done := (counter <= 1.U)
  }
}

package RISCV.implementation.RV32B

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._
import bitmanipulation.AbstractGeneralizedReverser
import bitmanipulation.AbstractShuffler
import bitmanipulation.AbstractSequentialRotater

class BitPermutationUnit(
    genGeneralizedReverser: () => AbstractGeneralizedReverser,
    genShuffler: () => AbstractShuffler,
    genRotater: () => AbstractSequentialRotater
) extends AbstractExecutionUnit(InstructionSets.BitPerm) {

  io.misa := "b01__0000__0_00000_00000_00000_00000_00010".U

  val generalizedReverser = Module(genGeneralizedReverser())
  val shuffler = Module(genShuffler())
  val rotater = Module(genRotater())

  val rd = io.instr(11, 7)
  val rs1 = io.instr(19, 15)
  val rs2 = Mux(RISCV_TYPE.getOP(io.instr_type) === RISCV_OP.OP_IMM, 0.U, io.instr(24, 20))
  val imm = Fill(20, io.instr(31)) ## io.instr(31, 20)

  io.stall := STALL_REASON.NO_STALL
  io_pc.pc_wdata := Mux(io.stall === STALL_REASON.EXECUTION_UNIT, io_pc.pc, io_pc.pc + 4.U)
  io_pc.pc_we := true.B

  io_reg.reg_rs1 := rs1
  io_reg.reg_rs2 := rs2
  io_reg.reg_rd := rd
  io_reg.reg_write_en := true.B

  io_reg.reg_write_data := 0.U

  generalizedReverser.io <> DontCare
  shuffler.io <> DontCare
  rotater.io <> DontCare

  rotater.io.start := false.B

  switch(io.instr_type) {
    is(RISCV_TYPE.grev) {
      generalizedReverser.io.input := io_reg.reg_read_data1
      generalizedReverser.io.pattern := io_reg.reg_read_data2
      io_reg.reg_write_data := generalizedReverser.io.result
    }
    is(RISCV_TYPE.grevi) {
      generalizedReverser.io.input := io_reg.reg_read_data1
      generalizedReverser.io.pattern := imm
      io_reg.reg_write_data := generalizedReverser.io.result
    }
    is(RISCV_TYPE.shfl) {
      shuffler.io.input := io_reg.reg_read_data1
      shuffler.io.pattern := io_reg.reg_read_data2
      shuffler.io.unshuffle := 0.U(1.W)
      io_reg.reg_write_data := shuffler.io.result
    }
    is(RISCV_TYPE.shfli) {
      shuffler.io.input := io_reg.reg_read_data1
      shuffler.io.pattern := imm
      shuffler.io.unshuffle := 0.U(1.W)
      io_reg.reg_write_data := shuffler.io.result
    }
    is(RISCV_TYPE.unshfl) {
      shuffler.io.input := io_reg.reg_read_data1
      shuffler.io.pattern := io_reg.reg_read_data2
      shuffler.io.unshuffle := 1.U(1.W)
      io_reg.reg_write_data := shuffler.io.result
    }
    is(RISCV_TYPE.unshfli) {
      shuffler.io.input := io_reg.reg_read_data1
      shuffler.io.pattern := imm
      shuffler.io.unshuffle := 1.U(1.W)
      io_reg.reg_write_data := shuffler.io.result
    }
    is(RISCV_TYPE.rol) {
      io.stall := Mux(rotater.io.done, STALL_REASON.NO_STALL, STALL_REASON.EXECUTION_UNIT)
      rotater.io.input := Reverse(io_reg.reg_read_data1)
      rotater.io.shamt := (io_reg.reg_read_data2(4, 0).asUInt & 31.U).asUInt
      rotater.io.start := true.B
      when(rotater.io.done) {
        io_reg.reg_write_data := Reverse(rotater.io.result)
      }
    }
    is(RISCV_TYPE.ror) {
      io.stall := Mux(rotater.io.done, STALL_REASON.NO_STALL, STALL_REASON.EXECUTION_UNIT)
      rotater.io.input := io_reg.reg_read_data1
      rotater.io.shamt := (io_reg.reg_read_data2(4, 0).asUInt & 31.U).asUInt
      rotater.io.start := true.B
      when(rotater.io.done) {
        io_reg.reg_write_data := rotater.io.result
      }
    }
    is(RISCV_TYPE.rori) {
      io.stall := Mux(rotater.io.done, STALL_REASON.NO_STALL, STALL_REASON.EXECUTION_UNIT)
      rotater.io.input := io_reg.reg_read_data1
      rotater.io.shamt := (imm(4, 0).asUInt & 31.U).asUInt
      rotater.io.start := true.B
      when(rotater.io.done) {
        io_reg.reg_write_data := rotater.io.result
      }
    }
  }

  io_data.data_req := false.B
  io_data.data_addr := 0.U
  io_data.data_be := 0.U
  io_data.data_we := false.B
  io_data.data_wdata := 0.U

  // Assign the trap interface
  io_trap.trap_valid := false.B
  io_trap.trap_reason := TRAP_REASON.NONE
}


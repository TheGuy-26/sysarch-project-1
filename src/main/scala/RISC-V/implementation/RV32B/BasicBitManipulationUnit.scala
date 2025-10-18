package RISCV.implementation.RV32B

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._
import bitmanipulation.AbstractLeadingZerosCounter

class BasicBitManipulationUnit(
    genLeadingZerosCounter: () => AbstractLeadingZerosCounter
) extends AbstractExecutionUnit(InstructionSets.BasicBit) {
  io.misa := "b01__0000__0_00000_00000_00000_00000_00010".U

  val leadingZerosCounter = Module(genLeadingZerosCounter())

  // exercise 2.4
  // Extracting registers from the instruction
  val rd = io.instr(11, 7)
  val rs1 = io.instr(19, 15)
  val rs2 = Mux(RISCV_TYPE.getOP(io.instr_type) === RISCV_OP.OP_IMM, 0.U, io.instr(24, 20))

  // default value for PC and STALL
  io.stall := STALL_REASON.NO_STALL
  io_pc.pc_wdata := io_pc.pc + 4.U
  io_pc.pc_we := true.B

  // default outputs for register file write
  io_reg.reg_write_en := true.B
  io_reg.reg_rd := rd
  io_reg.reg_rs1 := rs1
  io_reg.reg_rs2 := rs2
  io_reg.reg_write_data := 0.U

  io_data <> DontCare
  io_reset <> DontCare
  io_trap <> DontCare

  // Connecting the input of the leadingZerosCounter
  leadingZerosCounter.io.input := io_reg.reg_read_data1


  switch(instr_type) {
    is(RISCV_TYPE.clz) {
      io_reg.reg_write_data := leadingZerosCounter.io.result
    }
    is(RISCV_TYPE.ctz) {
      leadingZerosCounter.io.input := Reverse(io_reg.reg_read_data1)
      io_reg.reg_write_data := leadingZerosCounter.io.result
    }
    is(RISCV_TYPE.cpop) {
      io_reg.reg_write_data := PopCount(io_reg.reg_read_data1)
    }
    is(RISCV_TYPE.min) {
      val input_val1 = io_reg.reg_read_data1
      val input_val2 = io_reg.reg_read_data2

      io_reg.reg_write_data := Mux(input_val1.asSInt < input_val2.asSInt, input_val1, input_val2)
    }
    is(RISCV_TYPE.minu) {
      val input_val1 = io_reg.reg_read_data1
      val input_val2 = io_reg.reg_read_data2

      io_reg.reg_write_data := Mux(input_val1 < input_val2, input_val1, input_val2)
    }
    is(RISCV_TYPE.max) {
      val input_val1 = io_reg.reg_read_data1
      val input_val2 = io_reg.reg_read_data2

      io_reg.reg_write_data := Mux(input_val1.asSInt > input_val2.asSInt, input_val1, input_val2)
    }
    is(RISCV_TYPE.maxu) {
      val input_val1 = io_reg.reg_read_data1
      val input_val2 = io_reg.reg_read_data2

      io_reg.reg_write_data := Mux(input_val1 > input_val2, input_val1, input_val2)
    }
  }

}


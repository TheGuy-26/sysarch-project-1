package project1.public.RISCV.RV32B

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import RISCV.utils.PermBuilder.buildPermutation

class PermutationTest extends AnyFlatSpec with Matchers {

  behavior of "CustomPermutation"

  def performRotation(
      permutation: List[Int],
      immediate: Int
  ): List[Int] = {
    val n = immediate & 31
    permutation.drop(n) ++ permutation.take(n)
  }

  def performGeneralizedReverse(
      permutation: List[Int],
      immediate: Int
  ): List[Int] = {
    var new_permutation = permutation
    for (i <- 0 until 32) yield {
      var old_index = i
      if ((immediate & (1 << 4)) != 0)
        old_index = if (old_index >= 16) old_index - 16 else old_index + 16
      if ((immediate & (1 << 3)) != 0)
        old_index = if (old_index % 16 >= 8) old_index - 8 else old_index + 8
      if ((immediate & (1 << 2)) != 0)
        old_index = if (old_index % 8 >= 4) old_index - 4 else old_index + 4
      if ((immediate & (1 << 1)) != 0)
        old_index = if (old_index % 4 >= 2) old_index - 2 else old_index + 2
      if ((immediate & (1 << 0)) != 0)
        old_index = if (old_index % 2 == 1) old_index - 1 else old_index + 1

      new_permutation = new_permutation.updated(
        i,
        permutation(old_index)
      )
    }
    new_permutation

  }

  def performShuffle(
      permutation: List[Int],
      immediate: Int
  ): List[Int] = {
    var new_permutation = permutation
    for (i <- 0 until 32) yield {
      var old_index = i
      if ((immediate & (1 << 0)) != 0) old_index = old_index % 4 match {
        case 0 => old_index
        case 1 => old_index + 1
        case 2 => old_index - 1
        case 3 => old_index
      }
      if ((immediate & (1 << 1)) != 0) old_index = (old_index % 8 / 2) match {
        case 0 => old_index
        case 1 => old_index + 2
        case 2 => old_index - 2
        case 3 => old_index
      }
      if ((immediate & (1 << 2)) != 0) old_index = (old_index % 16 / 4) match {
        case 0 => old_index
        case 1 => old_index + 4
        case 2 => old_index - 4
        case 3 => old_index
      }
      if ((immediate & (1 << 3)) != 0) old_index = (old_index % 32 / 8) match {
        case 0 => old_index
        case 1 => old_index + 8
        case 2 => old_index - 8
        case 3 => old_index
      }
      new_permutation = new_permutation.updated(
        i,
        permutation(old_index)
      )
    }
    new_permutation
  }

  def performUnshuffle(
      permutation: List[Int],
      immediate: Int
  ): List[Int] = {
    var new_permutation = permutation
    for (i <- 0 until 32) yield {
      var old_index = i
      if ((immediate & (1 << 3)) != 0) old_index = (old_index % 32 / 8) match {
        case 0 => old_index
        case 1 => old_index + 8
        case 2 => old_index - 8
        case 3 => old_index
      }
      if ((immediate & (1 << 2)) != 0) old_index = (old_index % 16 / 4) match {
        case 0 => old_index
        case 1 => old_index + 4
        case 2 => old_index - 4
        case 3 => old_index
      }
      if ((immediate & (1 << 1)) != 0) old_index = (old_index % 8 / 2) match {
        case 0 => old_index
        case 1 => old_index + 2
        case 2 => old_index - 2
        case 3 => old_index
      }
      if ((immediate & (1 << 0)) != 0) old_index = old_index % 4 match {
        case 0 => old_index
        case 1 => old_index + 1
        case 2 => old_index - 1
        case 3 => old_index
      }
      new_permutation = new_permutation.updated(
        i,
        permutation(old_index)
      )
    }
    new_permutation

  }

  def parseImmediate(imm: String): Int = {
    try {
      if (imm.startsWith("0x")) BigInt(imm.substring(2), 16).toInt
      else imm.toInt
    } catch {
      case _: Exception =>
        throw new Exception(s"Invalid immediate value ${imm}.")
    }
  }

  def emulatePermutation(
      rd: Int,
      rs1: Int,
      instructions: List[String]
  ): Map[Int, Int] = {
    var permutation = List.range(0, 32)
    for (instruction <- instructions) {
      if (instruction.split(" ").length != 4) {
        throw new IllegalArgumentException(
          s"Invalid instruction format: $instruction"
        )
      }
      if (
        instruction
          .split(" ")(1) != s"x$rd" && instruction.split(" ")(1) != s"x$rd,"
      ) {
        throw new IllegalArgumentException(
          s"Invalid destination register. Make sure to use \"x$rd\": $instruction"
        )
      }
      if (
        instruction.split(" ")(2) != s"x$rs1" && instruction.split(" ")(
          2
        ) != s"x$rs1," && instruction.split(" ")(2) != s"x$rd" && instruction
          .split(" ")(2) != s"x$rd,"
      ) {
        throw new IllegalArgumentException(
          s"Invalid source register. Make sure to use \"x$rs1\" or \"x$rd\": $instruction"
        )
      }
      val opcode = instruction.split(" ")(0)
      val immediate = parseImmediate(instruction.split(" ")(3))
      permutation = opcode match {
        case "rori"    => performRotation(permutation, immediate)
        case "roli"    => performRotation(permutation, 32 - immediate % 32)
        case "grevi"   => performGeneralizedReverse(permutation, immediate)
        case "shfli"   => performShuffle(permutation, immediate)
        case "unshfli" => performUnshuffle(permutation, immediate)
        case _ =>
          throw new IllegalArgumentException(s"Unknown instruction: $opcode")
      }
    }
    permutation.zipWithIndex.toMap

  }

  it should "perform a simple rotation correctly" in {
    val permutation = List(31, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
      15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
      30).zipWithIndex.map(_.swap).toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }
  it should "perform no bit manipulation" in {
    val permutation = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
      31).zipWithIndex.map(_.swap).toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  it should "perform a generalized reverse correctly" in {
    val permutation = List(1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14,
      17, 16, 19, 18, 21, 20, 23, 22, 25, 24, 27, 26, 29, 28, 31,
      30).zipWithIndex.map(_.swap).toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  it should "perform a shuffle correctly" in {
    val permutation = List(0, 2, 1, 3, 4, 6, 5, 7, 8, 10, 9, 11, 12, 14, 13, 15,
      16, 18, 17, 19, 20, 22, 21, 23, 24, 26, 25, 27, 28, 30, 29,
      31).zipWithIndex.map(_.swap).toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

    it should "perform random permutation correctly" in {
      val permutation = List(17, 29, 4, 20, 6, 11, 24, 0, 10, 13, 25, 31, 1, 8, 21, 15, 2, 7, 30, 5, 19, 16, 18, 26, 14, 27, 9, 3, 28, 23, 22, 12).zipWithIndex.map(_.swap).toMap
      val instructions = buildPermutation(1, 2, permutation)
      val result = emulatePermutation(1, 2, instructions)
      result shouldEqual permutation
    }

  for (i <- 1 until 32) {
    it should s"swap bits 0 and $i correctly" in {
      val base = (0 until 32).toList
      val permutation = base.updated(0, i).updated(i, 0).zipWithIndex.map(_.swap).toMap
      val instructions = buildPermutation(1, 2, permutation)
      val result = emulatePermutation(1, 2, instructions)
      result shouldEqual permutation
    }
  }

  it should "swap bits 5 and 6 and 9 correctly" in {
    val permutation = List(0, 1, 2, 3, 4, 6, 9, 7, 8, 5, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
      31).zipWithIndex.map(_.swap).toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

    it should "swap bits 5 and 6 correctly" in {
    val permutation = List(0, 1, 2, 3, 4, 6, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
      31).zipWithIndex.map(_.swap).toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  // tests for lookup table
  it should "perform bit-reverse and rotate right by 4 correctly" in {
    val permutation = (0 until 32).map(i => i -> ((31 - i + 4) % 32)).toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  it should "perform AES MixColumns pattern (4x4 transpose) correctly" in {
    val permutation = (0 until 32).flatMap { i =>
      val group = i / 8
      val posInGroup = i % 8
      if (posInGroup < 4) Seq(i -> (group * 8 + posInGroup + 4))
      else Seq(i -> (group * 8 + posInGroup - 4))
    }.toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  it should "perform byte swap and nibble swap correctly" in {
    val permutation = (0 until 32).map { i =>
      val bytePos = i / 8
      val bitInByte = i % 8
      val newBytePos = 3 - bytePos
      val newBitInByte = if (bitInByte < 4) bitInByte + 4 else bitInByte - 4
      i -> (newBytePos * 8 + newBitInByte)
    }.toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  it should "perform FFT bit interleaving (odd/even separation) correctly" in {
    val permutation = (0 until 32).map { i =>
      val newPos = if (i < 16) i * 2 else (i - 16) * 2 + 1
      i -> newPos
    }.toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  it should "perform DES initial permutation (32 bit) correctly" in {
    val permutation = Map(
      0->4, 1->5, 2->6, 3->7, 4->0, 5->1, 6->2, 7->3,
      8->12, 9->13, 10->14, 11->15, 12->8, 13->9, 14->10, 15->11,
      16->20, 17->21, 18->22, 19->23, 20->16, 21->17, 22->18, 23->19,
      24->28, 25->29, 26->30, 27->31, 28->24, 29->25, 30->26, 31->27
    )
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  it should "perform CRC32 bit mirror (4-bit blocks) correctly" in {
    val permutation = (0 until 32).map { i =>
      val chunk = i / 4
      val posInChunk = i % 4
      i -> (chunk * 4 + (3 - posInChunk))
    }.toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

//  it should "perform matrix transposition (8x4 bit matrix) correctly" in {
//    val permutation = (0 until 32).map { i =>
//      val row = i / 8
//      val col = i % 8
//      val newRow = col / 4
//      val newCol = (col % 4) * 2 + row
//      i -> (newRow * 8 + newCol)
//    }.toMap
//    val instructions = buildPermutation(1, 2, permutation)
//    val result = emulatePermutation(1, 2, instructions)
//    result shouldEqual permutation
//  }

  it should "perform zigzag reordering correctly" in {
    val permutation = Map(
      0->0, 1->1, 2->8, 3->16, 4->9, 5->2, 6->3, 7->10,
      8->17, 9->24, 10->25, 11->18, 12->11, 13->4, 14->5, 15->12,
      16->19, 17->26, 18->27, 19->20, 20->13, 21->6, 22->7, 23->14,
      24->21, 25->28, 26->29, 27->22, 28->15, 29->23, 30->30, 31->31
    )
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  it should "perform hadamard reordering correctly" in {
    val permutation = (0 until 32).map { i =>
      val rev5 = Integer.reverse(i) >>> 27
      val newPos = rev5 ^ (rev5 >> 1)
      i -> newPos
    }.toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  // more tests
  it should "perform some random permuation 1" in {
    val permutation = List(21, 17, 2, 15, 4, 5, 6, 29, 8, 9, 10, 11, 12, 13, 14, 3,
      16, 1, 18, 19, 20, 0, 22, 23, 24, 25, 26, 27, 28, 7, 30,
      31).zipWithIndex.map(_.swap).toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  it should "perform swap of the last two bits" in {
    val permutation = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31,
      30).zipWithIndex.map(_.swap).toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }

  it should "perform swap of the first and the last bit" in {
    val permutation = List(31, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
      0).zipWithIndex.map(_.swap).toMap
    val instructions = buildPermutation(1, 2, permutation)
    val result = emulatePermutation(1, 2, instructions)
    result shouldEqual permutation
  }
}

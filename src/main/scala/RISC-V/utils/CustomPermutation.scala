package RISCV.utils
import scala.util.control.Breaks._

object PermBuilder {

  /** This function takes a mapping for the permutation and returns the list of
    * necessary instructions to implement the permutation.
    *
    * You may assume that the map encodes a valid permutation, i.e., that every
    * destination bit is associated with a unique source bit.
    *
    * You may only write to the register rd.
    *
    * @param rd
    *   The destination register
    * @param rs1
    *   The source register
    * @param perm
    *   A map from representing the permutation, mapping destination bit
    *   positions to source bit positions.
    * @return
    *   A list of strings representing the instructions to implement the
    *   permutation e.g. List("grevi x1, x2, 0x01", "grevi x1, x2, 0x02", ...)
    */
  def buildPermutation(rd: Int, rs1: Int, perm: Map[Int, Int]): List[String] = {
    val list = mapToList(perm)
    var out = List.empty[String]
    val originalDistance = calculateDistance(list)

    if (originalDistance > 0.0) {
      val (bestNextInstr, newDistance) = findBestNextInstr(rd, rs1, list, originalDistance)
      if (newDistance == 0.0)
        out = bestNextInstr :: out
      else
        out = swapByIndices(rd, rs1, list)
    }
    out
  }

  def swapByIndices(rd: Int, rs: Int, perm: List[Int]): List[String] = {
    var newPerm = perm
    var instructions = List(s"rori x$rd x$rs 0")
    var k = 0
    for ( i <- 0 until 32) {
      k = i
      var destination = newPerm(k)
      while (destination != k) {
        newPerm = newPerm.updated(k, newPerm(destination)).updated(destination, newPerm(k))
        instructions = instructions ::: rotateNAndSwap(rd, math.min(k, destination), math.abs(destination - k))
        destination = newPerm(k)
      }
    }
    instructions
  }

  def swapZeroAndN(rd: Int, n: Int): List[String] = {
    n match {
      case 0 => List.empty[String]
      case 1 => swapZeroAndOne(rd)
      case 2 => List(s"shfli x$rd x$rd 1") ::: swapZeroAndOne(rd) ::: List(s"shfli x$rd x$rd 1")
      case 3 => List(s"grevi x$rd x$rd 1", s"rori x$rd x$rd 1") ::: swapZeroAndOne(rd) ::: List(s"rori x$rd x$rd 31", s"grevi x$rd x$rd 1")
      case 4 => List(s"shfli x$rd x$rd 3") ::: swapZeroAndOne(rd) ::: List(s"unshfli x$rd x$rd 3")
      case 5 => List(s"grevi x$rd x$rd 4", s"rori x$rd x$rd 1") ::: swapZeroAndN(rd, 3) ::: List(s"rori x$rd x$rd 31", s"grevi x$rd x$rd 4")
      case 6 => List(s"grevi x$rd x$rd 2", s"rori x$rd x$rd 2") ::: swapZeroAndN(rd, 2) ::: List(s"rori x$rd x$rd 30", s"grevi x$rd x$rd 2")
      case 7 => List(s"grevi x$rd x$rd 4", s"rori x$rd x$rd 3") ::: swapZeroAndOne(rd) ::: List(s"rori x$rd x$rd 29", s"grevi x$rd x$rd 4")
      case 8 => List(s"shfli x$rd x$rd 7") ::: swapZeroAndOne(rd) ::: List(s"unshfli x$rd x$rd 7")
      case 9 => List(s"shfli x$rd x$rd 6") ::: swapZeroAndN(rd, 3) ::: List(s"unshfli x$rd x$rd 6")
      case 10 => List(s"shfli x$rd x$rd 6") ::: swapZeroAndN(rd, 6) ::: List(s"unshfli x$rd x$rd 6")
      case 11 => List(s"shfli x$rd x$rd 4") ::: swapZeroAndN(rd, 7) ::: List(s"unshfli x$rd x$rd 4")
      case 12 => List(s"shfli x$rd x$rd 3") ::: swapZeroAndN(rd, 9) ::: List(s"unshfli x$rd x$rd 3")
      case 13 => List(s"unshfli x$rd x$rd 6") ::: swapZeroAndN(rd, 7) ::: List(s"shfli x$rd x$rd 6")
      case 14 => List(s"unshfli x$rd x$rd 7") ::: swapZeroAndN(rd, 7) ::: List(s"shfli x$rd x$rd 7")
      case 15 => List(s"grevi x$rd x$rd 7", s"rori x$rd x$rd 7") ::: swapZeroAndOne(rd)::: List(s"rori x$rd x$rd 25", s"grevi x$rd x$rd 7")
      case 16 => List(s"shfli x$rd x$rd 15") ::: swapZeroAndOne(rd) ::: List(s"unshfli x$rd x$rd 15")
      case 17 => List(s"shfli x$rd x$rd 14") ::: swapZeroAndN(rd, 3) ::: List(s"unshfli x$rd x$rd 14")
      case 18 => List(s"shfli x$rd x$rd 12") ::: swapZeroAndN(rd, 6) ::: List(s"unshfli x$rd x$rd 12")
      case 19 => List(s"shfli x$rd x$rd 12") ::: swapZeroAndN(rd, 7) ::: List(s"unshfli x$rd x$rd 12")
      case 20 => List(s"shfli x$rd x$rd 15") ::: swapZeroAndN(rd, 9) ::: List(s"unshfli x$rd x$rd 15")
      case 21 => List(s"rori x$rd x$rd 21") ::: swapZeroAndN(rd, 11) ::: List(s"rori x$rd x$rd 11")
      case 22 => List(s"rori x$rd x$rd 22") ::: swapZeroAndN(rd, 10) ::: List(s"rori x$rd x$rd 10")
      case 23 => List(s"rori x$rd x$rd 23") ::: swapZeroAndN(rd, 9) ::: List(s"rori x$rd x$rd 9")
      case 24 => List(s"rori x$rd x$rd 24") ::: swapZeroAndN(rd, 8) ::: List(s"rori x$rd x$rd 8")
      case 25 => List(s"rori x$rd x$rd 25") ::: swapZeroAndN(rd, 7) ::: List(s"rori x$rd x$rd 7")
      case 26 => List(s"rori x$rd x$rd 26") ::: swapZeroAndN(rd, 6) ::: List(s"rori x$rd x$rd 6")
      case 27 => List(s"rori x$rd x$rd 27") ::: swapZeroAndN(rd, 5) ::: List(s"rori x$rd x$rd 5")
      case 28 => List(s"rori x$rd x$rd 28") ::: swapZeroAndN(rd, 4) ::: List(s"rori x$rd x$rd 4")
      case 29 => List(s"rori x$rd x$rd 29") ::: swapZeroAndN(rd, 3) ::: List(s"rori x$rd x$rd 3")
      case 30 => List(s"rori x$rd x$rd 30") ::: swapZeroAndN(rd, 2) ::: List(s"rori x$rd x$rd 2")
      case 31 => List(s"rori x$rd x$rd 31") ::: swapZeroAndOne(rd) ::: List(s"rori x$rd x$rd 1")
    }
  }

  def rotateNAndSwap(rd: Int, n: Int, swapOffset: Int): List[String] = {
    if (n == 0)
      swapZeroAndN(rd, swapOffset)
    val minusN = 32 - n
    List(s"rori x$rd x$rd $n") ::: swapZeroAndN(rd, swapOffset) ::: List(s"rori x$rd x$rd $minusN")
  }

  def swapZeroAndOne(rd: Int): List[String] = {
    List(
      s"rori x$rd x$rd 2",
      s"unshfli x$rd x$rd 31",
      s"rori x$rd x$rd 31",
      s"shfli x$rd x$rd 31"
    )
  }

  def findBestNextInstr(rd: Int, rs1: Int, perm: List[Int], dist: Double): (String, Double) = {
    var instr = ""
    var distance = dist

    val (bestRotationOffset, bestRotationDistance) = findBestRotation(perm)
    if (bestRotationDistance == 0.0) {
      instr = s"rori x$rd x$rs1 $bestRotationOffset"
      distance = bestRotationDistance
    } else {
      val (bestGeneralizedReverseOffset, bestGeneralizedReverseDistance) =
        findBestGeneralizedReverse(perm)
      if (bestGeneralizedReverseDistance == 0.0) {
        instr = s"grevi x$rd x$rs1 $bestGeneralizedReverseOffset"
        distance = bestGeneralizedReverseDistance
      } else {
        val (bestShuffleOffset, bestShuffleDistance) = findBestShuffle(perm)
        if (bestShuffleDistance == 0.0) {
          instr = s"shfli x$rd x$rs1 $bestShuffleOffset"
          distance = bestShuffleDistance
        } else {
          val (bestUnshuffleOffset, bestUnshuffleDistance) = findBestUnshuffle(
            perm
          )
          if (bestUnshuffleDistance == 0.0) {
            instr = s"unshfli x$rd x$rs1 $bestUnshuffleOffset"
            distance = bestUnshuffleDistance
          }
        }
      }
    }
    (instr, distance)
  }

  def findBestShuffle(org: List[Int]): (Int, Double) = {
    var minDistance = calculateDistance(org)
    var out = 0

    breakable {
      for (i <- 1 until 16) {
        val rotated = performShuffle(org, i)
        val newDistance = calculateDistance(rotated)
        if (newDistance < minDistance) {
          minDistance = newDistance
          out = i
        }
        if (minDistance == 0.0) break()
      }
    }
    (out, minDistance)
  }

  def findBestUnshuffle(org: List[Int]): (Int, Double) = {
    var minDistance = calculateDistance(org)
    var out = 0
    breakable {
      for (i <- 1 until 16) {
        val rotated = performUnshuffle(org, i)
        val newDistance = calculateDistance(rotated)
        if (newDistance < minDistance) {
          minDistance = newDistance
          out = i
        }
        if (minDistance == 0.0) break()
      }
    }
    (out, minDistance)
  }

  def findBestGeneralizedReverse(org: List[Int]): (Int, Double) = {
    var minDistance = calculateDistance(org)
    var out = 0
    breakable {
      for (i <- 1 until 32) {
        val rotated = performGeneralizedReverse(org, i)
        val newDistance = calculateDistance(rotated)
        if (newDistance < minDistance) {
          minDistance = newDistance
          out = i
        }
        if (minDistance == 0.0) break()
      }
    }
    (out, minDistance)
  }

  def findBestRotation(org: List[Int]): (Int, Double) = {
    var minDistance = calculateDistance(org)
    var out = 0
    breakable {
      for (i <- 1 until 32) {
        val rotated = performRotation(org, i)
        val newDistance = calculateDistance(rotated)
        if (newDistance < minDistance) {
          minDistance = newDistance
          out = i
        }
        if (minDistance == 0.0) break()
      }
    }
    (out, minDistance)
  }

  def mapToList(map: Map[Int, Int]): List[Int] = {
    var list = List.empty[Int]
    for (i <- (0 until 32).reverse) {
      list = map.getOrElse(i, 0) :: list
    }
    list
  }

  def calculateDistance(perm: List[Int]): Double = {
    var distance = 0.0
    for (k <- 0 until 32) {
      val j = perm(k)
      val i = k
      distance += math.pow(j - i, 2)
//      println(s"MSE = $distance")
    }
//    println(s"MSE = $distance")
    distance = math.sqrt(distance)
    distance
  }

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

  def calculateDistance(perm: Map[Int, Int]): Double = {
    var distance = 0.0
    for (k <- 0 until 32) {
      val j = perm.getOrElse(k, 0)
      val i = k
      distance += math.pow(j - i, 2)
//      println(s"MSE = $distance")
    }
//    println(s"MSE = $distance")
    distance = math.sqrt(distance)
    distance
  }
}

// See README.md for license details.

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

/**
 * Simple Passthrough module
 * @param proto  The type generator for the swapper
 * @tparam T     The type of the inputs and outputs, derived from proto
 */
class Passthrough[T <: Data](proto: T) extends Module {
  val io = IO(new Bundle {
    val a0 = Input(proto)
    val a1 = Input(proto)
    val z0 = Output(proto)
    val z1 = Output(proto)
  })

  io.z0 := io.a0
  io.z1 := io.a1

}

/**
 * Bitonic Sorter is a factory for combinational sort hardware.
 * It requires that the elements to be sorted are a power of 2.
 *
 * @see https://en.wikipedia.org/wiki/Bitonic_sorter
 */
object BitonicSorter {
  def bitonicMerge[T <: Data](arr: IndexedSeq[Option[T]], length: Int, ascending: Bool, lt: (T, T) => Bool): IndexedSeq[Option[T]] = {
    def swapIfNecessary(seq: IndexedSeq[Option[T]], i: Int, ascending: Bool, lt: (T, T) => Bool): IndexedSeq[Option[T]] = {
      val j = i + length / 2
      val si = arr(i).getOrElse(0.U.asTypeOf(arr(i).get))
      val sj = arr(j).getOrElse(0.U.asTypeOf(arr(j).get))
      val m = Module(new Passthrough(si.cloneType))
      val swapNeeded = lt(sj, si) === ascending
      when(swapNeeded) {
        m.io.a0 := sj
        m.io.a1 := si
      }.otherwise {
        m.io.a0 := si
        m.io.a1 := sj
      }
      seq.updated(i, Some(m.io.z0)).updated(j, Some(m.io.z1))
    }

    if (length > 1) {
      val half = length / 2
      val indices = 0 until half

      val swappedSeq = indices.foldLeft(arr) { (seq, i) =>
        swapIfNecessary(seq, i, ascending, lt)
      }

      val left = bitonicMerge(swappedSeq.slice(0, half), half, ascending, lt) // Pass only the first half
      val right = bitonicMerge(swappedSeq.slice(half, length), half, ascending, lt) // Pass only the second half
      left ++ right
    }
    else {
      arr
    }
  }

  def bitonicSort[T <: Data](arr: IndexedSeq[Option[T]], ascending: Bool, lt: (T, T) => Bool): IndexedSeq[Option[T]] = {
    val n = arr.length
    if (n > 1) {
      val half = n / 2
      val left = bitonicSort(arr.slice(0, half), true.B, lt) // Pass only the first half
      val right = bitonicSort(arr.slice(half, n), false.B, lt) // Pass only the second half
      val merged = bitonicMerge(left ++ right, n, ascending, lt)
      merged
    } else {
      arr
    }
  }

  def sort[T <: Data](a: IndexedSeq[Option[T]], ascending: Bool = true.B, lt: (T, T) => Bool): IndexedSeq[Option[T]] = {
    if (a.length <= 1) {
      // Nothing to sort, return the input array
      a
    }
    else {
      assert((1 << log2Up(a.length)) == a.length, "Array length must be a power of 2")
      val sortedArray = bitonicSort(a, ascending, lt)
      sortedArray
    }
  }
}

/**
 * Defines the interface used for the Bitonic Sort module
 * @param n      The number of elements
 * @param proto  An instance of the type of all elements
 * @tparam T     The type as derived from proto
 */
class SorterModuleIfc[T <: Data](val n: Int, proto: T) extends Module {
  val io = IO(new Bundle {
    val a = Input(Vec(n, proto.cloneType))
    val z = Output(Vec(n, proto.cloneType))
  })
}

/*
 * What follows are some example code generation calls.
 */

class BitonicSorterModule[T <: Data](n: Int, proto: T, ascending: Bool, lt: (T, T) => Bool) extends SorterModuleIfc(n, proto) {
  if ((1 << log2Up(io.a.length)) != io.a.length && io.a.length > 1) {
    throw new IllegalArgumentException("Array length must be a power of 2")
  }
  private val a = IndexedSeq.tabulate(if (n <= 1) 1 else 1 << log2Up(io.a.length)) { i => if (i < n) Some(io.a(i)) else None }  //special case is a sequence of length 1
  io.z := VecInit(BitonicSorter.sort(a, ascending, lt).map(_.get))
}

object BitonicSorterUInt8_64Driver extends App {
  (new ChiselStage).emitSystemVerilog(new BitonicSorterModule(64, UInt(8.W), true.B, (x: UInt, y: UInt) => x < y), args)
}

object BitonicSorterUInt8_384Driver extends App {
  (new ChiselStage).emitSystemVerilog(new BitonicSorterModule(384, UInt(8.W), true.B, (x: UInt, y: UInt) => x < y), args)
}


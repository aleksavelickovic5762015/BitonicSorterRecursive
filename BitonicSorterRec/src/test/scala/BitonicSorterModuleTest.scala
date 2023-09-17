import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random // Import the Random class


class BitonicSorterModuleTest extends AnyFlatSpec with ChiselScalatestTester {
  def generateRandomInput(size: Int, len: Int): Seq[UInt] = {
    Seq.fill(len)(BigInt(size, Random).U(size.W))
  }
  "BitonicSorterModule" should "sort input values in ascending order" in {
    val bitSizes = Seq(8, 16, 32, 64)

    //length of an input seq
    val lengths = Seq(1, 2, 4, 8, 16)
    val randomIndex = Random.nextInt(lengths.length)
    val length = lengths(randomIndex)

    for (bitSize <- bitSizes) {
      test(new BitonicSorterModule(length, UInt(bitSize.W), true.B, (x: UInt, y: UInt) => x < y)) { dut =>
        val input = generateRandomInput(bitSize, length)

        //println("\n"+bitSize+"-bit")
        dut.io.a.zipWithIndex.foreach { case (inputPort, index) =>
          inputPort.poke(input(index))
        }

        //dut.clock.step()

        // Check if the output values are in ascending order
        var prevValue = BigInt(0)
        dut.io.z.foreach { outputPort: UInt =>
          val value = outputPort.peek().litValue//.toInt
          //println(value)
          outputPort.expect(value.U)
          assert(value >= prevValue, "Output values are not in ascending order.")
          prevValue = value
        }
      }
    }
  }

  "BitonicSorterModule" should "sort input values in descending order" in {
    val bitSizes = Seq(8, 16, 32, 64)

    //length of an input seq
    val lengths = Seq(1, 2, 4, 8, 16)
    val randomIndex = Random.nextInt(lengths.length)
    val length = lengths(randomIndex)

    for (bitSize <- bitSizes) {
      test(new BitonicSorterModule(length, UInt(bitSize.W), false.B, (x: UInt, y: UInt) => x < y)) { dut =>
        val input = generateRandomInput(bitSize, length)

        //println("\n" + bitSize + "-bit")
        dut.io.a.zipWithIndex.foreach { case (inputPort, index) =>
          inputPort.poke(input(index))
        }

        //dut.clock.step()  //Sequential circuit, clk not needed

        // Check if the output values are in descending order
        val veryLargeNumber = BigInt("922337203685477580755555")
        var prevValue = veryLargeNumber
        dut.io.z.foreach { outputPort: UInt =>
          val value = outputPort.peek().litValue //.toInt
          //println(value)
          outputPort.expect(value.U)
          assert(value <= prevValue, "Output values are not in descending order.")
          prevValue = value
        }
      }
    }
  }

  it should "not allow construction of Arrays whose length is not a power of 2" in {
    assertThrows[IllegalArgumentException] { new BitonicSorterModule(7, UInt(8.W), true.B, (x: UInt, y: UInt) => x < y) }
    assertThrows[IllegalArgumentException] { new BitonicSorterModule(9, UInt(8.W), true.B, (x: UInt, y: UInt) => x < y) }
    assertThrows[IllegalArgumentException] { new BitonicSorterModule(10, UInt(8.W), true.B, (x: UInt, y: UInt) => x < y) }
    assertThrows[IllegalArgumentException] { new BitonicSorterModule(21, UInt(8.W), true.B, (x: UInt, y: UInt) => x < y) }
    assertThrows[IllegalArgumentException] { new BitonicSorterModule(30, UInt(8.W), true.B, (x: UInt, y: UInt) => x < y) }
    assertThrows[IllegalArgumentException] { new BitonicSorterModule(43, UInt(8.W), true.B, (x: UInt, y: UInt) => x < y) }
  }
}
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BitonicSorterModuleTest extends AnyFlatSpec with ChiselScalatestTester {
  "BitonicSorterModule" should "sort input values in ascending order" in {
    test(new BitonicSorterModule(8, UInt(8.W), true.B, (x: UInt, y: UInt) => x < y)) { dut =>
      val input = Seq(5, 8, 3, 1, 6, 2, 7, 4).map(_.U)

      dut.io.a.zipWithIndex.foreach { case (inputPort, index) =>
        inputPort.poke(input(index))
      }

      dut.clock.step()

      // Check if the output values are in ascending order
      var prevValue = BigInt(0)
      dut.io.z.foreach { outputPort: UInt =>
        val value = outputPort.peek().litValue.toInt
        println(value)
        outputPort.expect(value.U)
        assert(value >= prevValue, "Output values are not in ascending order.")
        prevValue = value
      }
    }
  }
}

FIRRTL version 1.1.0
circuit BitonicSorterModule :
  module BitonicSorterModule :
    input clock : Clock
    input reset : UInt<1>
    input io_a_0 : UInt<64>
    output io_z_0 : UInt<64>

    node _WIRE_0 = io_a_0 @[BitonicSorterModule.scala 114:{18,18}]
    io_z_0 <= _WIRE_0 @[BitonicSorterModule.scala 114:8]

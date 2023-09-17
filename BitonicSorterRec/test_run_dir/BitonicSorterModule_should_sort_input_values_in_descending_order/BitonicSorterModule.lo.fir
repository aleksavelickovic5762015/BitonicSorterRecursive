FIRRTL version 1.1.0
circuit BitonicSorterModule :
  module Passthrough :
    input clock : Clock
    input reset : UInt<1>
    input io_a0 : UInt<64>
    input io_a1 : UInt<64>
    output io_z0 : UInt<64>
    output io_z1 : UInt<64>

    io_z0 <= io_a0 @[BitonicSorterModule.scala 20:9]
    io_z1 <= io_a1 @[BitonicSorterModule.scala 21:9]

  module BitonicSorterModule :
    input clock : Clock
    input reset : UInt<1>
    input io_a_0 : UInt<64>
    input io_a_1 : UInt<64>
    input io_a_2 : UInt<64>
    input io_a_3 : UInt<64>
    output io_z_0 : UInt<64>
    output io_z_1 : UInt<64>
    output io_z_2 : UInt<64>
    output io_z_3 : UInt<64>

    inst sortedArray_left_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    node _sortedArray_left_merged_swappedSeq_swapNeeded_T = lt(io_a_1, io_a_0) @[BitonicSorterModuleTest.scala 53:94]
    node sortedArray_left_merged_swappedSeq_swapNeeded = eq(_sortedArray_left_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_0 = mux(sortedArray_left_merged_swappedSeq_swapNeeded, io_a_1, io_a_0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_1 = mux(sortedArray_left_merged_swappedSeq_swapNeeded, io_a_0, io_a_1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_swappedSeq_swapNeeded_T = lt(io_a_3, io_a_2) @[BitonicSorterModuleTest.scala 53:94]
    node sortedArray_right_merged_swappedSeq_swapNeeded = eq(_sortedArray_right_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_2 = mux(sortedArray_right_merged_swappedSeq_swapNeeded, io_a_3, io_a_2) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_3 = mux(sortedArray_right_merged_swappedSeq_swapNeeded, io_a_2, io_a_3) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T = lt(sortedArray_right_merged_swappedSeq_m.io_z0, sortedArray_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 53:94]
    node sortedArray_merged_swappedSeq_swapNeeded = eq(_sortedArray_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_4 = mux(sortedArray_merged_swappedSeq_swapNeeded, sortedArray_right_merged_swappedSeq_m.io_z0, sortedArray_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_5 = mux(sortedArray_merged_swappedSeq_swapNeeded, sortedArray_left_merged_swappedSeq_m.io_z0, sortedArray_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T_1 = lt(sortedArray_right_merged_swappedSeq_m.io_z1, sortedArray_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 53:94]
    node sortedArray_merged_swappedSeq_swapNeeded_1 = eq(_sortedArray_merged_swappedSeq_swapNeeded_T_1, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_6 = mux(sortedArray_merged_swappedSeq_swapNeeded_1, sortedArray_right_merged_swappedSeq_m.io_z1, sortedArray_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_7 = mux(sortedArray_merged_swappedSeq_swapNeeded_1, sortedArray_left_merged_swappedSeq_m.io_z1, sortedArray_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_swappedSeq_swapNeeded_T = lt(sortedArray_merged_swappedSeq_m_1.io_z0, sortedArray_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 53:94]
    node sortedArray_merged_left_swappedSeq_swapNeeded = eq(_sortedArray_merged_left_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_8 = mux(sortedArray_merged_left_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m_1.io_z0, sortedArray_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_9 = mux(sortedArray_merged_left_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m.io_z0, sortedArray_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_swappedSeq_swapNeeded_T = lt(sortedArray_merged_swappedSeq_m_1.io_z1, sortedArray_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 53:94]
    node sortedArray_merged_right_swappedSeq_swapNeeded = eq(_sortedArray_merged_right_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_10 = mux(sortedArray_merged_right_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m_1.io_z1, sortedArray_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_11 = mux(sortedArray_merged_right_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m.io_z1, sortedArray_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _WIRE_0 = sortedArray_merged_left_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 114:{18,18}]
    node _WIRE_1 = sortedArray_merged_left_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 114:{18,18}]
    node _WIRE_2 = sortedArray_merged_right_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 114:{18,18}]
    node _WIRE_3 = sortedArray_merged_right_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 114:{18,18}]
    io_z_0 <= _WIRE_0 @[BitonicSorterModule.scala 114:8]
    io_z_1 <= _WIRE_1 @[BitonicSorterModule.scala 114:8]
    io_z_2 <= _WIRE_2 @[BitonicSorterModule.scala 114:8]
    io_z_3 <= _WIRE_3 @[BitonicSorterModule.scala 114:8]
    sortedArray_left_merged_swappedSeq_m.clock <= clock
    sortedArray_left_merged_swappedSeq_m.reset <= reset
    sortedArray_left_merged_swappedSeq_m.io_a0 <= _GEN_0
    sortedArray_left_merged_swappedSeq_m.io_a1 <= _GEN_1
    sortedArray_right_merged_swappedSeq_m.clock <= clock
    sortedArray_right_merged_swappedSeq_m.reset <= reset
    sortedArray_right_merged_swappedSeq_m.io_a0 <= _GEN_2
    sortedArray_right_merged_swappedSeq_m.io_a1 <= _GEN_3
    sortedArray_merged_swappedSeq_m.clock <= clock
    sortedArray_merged_swappedSeq_m.reset <= reset
    sortedArray_merged_swappedSeq_m.io_a0 <= _GEN_4
    sortedArray_merged_swappedSeq_m.io_a1 <= _GEN_5
    sortedArray_merged_swappedSeq_m_1.clock <= clock
    sortedArray_merged_swappedSeq_m_1.reset <= reset
    sortedArray_merged_swappedSeq_m_1.io_a0 <= _GEN_6
    sortedArray_merged_swappedSeq_m_1.io_a1 <= _GEN_7
    sortedArray_merged_left_swappedSeq_m.clock <= clock
    sortedArray_merged_left_swappedSeq_m.reset <= reset
    sortedArray_merged_left_swappedSeq_m.io_a0 <= _GEN_8
    sortedArray_merged_left_swappedSeq_m.io_a1 <= _GEN_9
    sortedArray_merged_right_swappedSeq_m.clock <= clock
    sortedArray_merged_right_swappedSeq_m.reset <= reset
    sortedArray_merged_right_swappedSeq_m.io_a0 <= _GEN_10
    sortedArray_merged_right_swappedSeq_m.io_a1 <= _GEN_11

FIRRTL version 1.1.0
circuit BitonicSorterModule :
  module Passthrough :
    input clock : Clock
    input reset : UInt<1>
    input io_a0 : UInt<16>
    input io_a1 : UInt<16>
    output io_z0 : UInt<16>
    output io_z1 : UInt<16>

    io_z0 <= io_a0 @[BitonicSorterModule.scala 20:9]
    io_z1 <= io_a1 @[BitonicSorterModule.scala 21:9]

  module BitonicSorterModule :
    input clock : Clock
    input reset : UInt<1>
    input io_a_0 : UInt<16>
    input io_a_1 : UInt<16>
    input io_a_2 : UInt<16>
    input io_a_3 : UInt<16>
    input io_a_4 : UInt<16>
    input io_a_5 : UInt<16>
    input io_a_6 : UInt<16>
    input io_a_7 : UInt<16>
    input io_a_8 : UInt<16>
    input io_a_9 : UInt<16>
    input io_a_10 : UInt<16>
    input io_a_11 : UInt<16>
    input io_a_12 : UInt<16>
    input io_a_13 : UInt<16>
    input io_a_14 : UInt<16>
    input io_a_15 : UInt<16>
    output io_z_0 : UInt<16>
    output io_z_1 : UInt<16>
    output io_z_2 : UInt<16>
    output io_z_3 : UInt<16>
    output io_z_4 : UInt<16>
    output io_z_5 : UInt<16>
    output io_z_6 : UInt<16>
    output io_z_7 : UInt<16>
    output io_z_8 : UInt<16>
    output io_z_9 : UInt<16>
    output io_z_10 : UInt<16>
    output io_z_11 : UInt<16>
    output io_z_12 : UInt<16>
    output io_z_13 : UInt<16>
    output io_z_14 : UInt<16>
    output io_z_15 : UInt<16>

    inst sortedArray_left_left_left_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_left_right_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_left_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_left_merged_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_left_merged_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_left_merged_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_right_left_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_right_right_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_right_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_right_merged_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_right_merged_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_right_merged_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_swappedSeq_m_2 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_swappedSeq_m_3 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_left_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_left_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_left_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_right_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_right_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_right_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_left_left_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_left_right_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_left_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_left_merged_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_left_merged_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_left_merged_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_right_left_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_right_right_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_right_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_right_merged_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_right_merged_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_right_merged_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_swappedSeq_m_2 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_swappedSeq_m_3 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_left_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_left_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_left_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_right_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_right_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_right_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m_2 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m_3 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m_4 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m_5 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m_6 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m_7 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_swappedSeq_m_2 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_swappedSeq_m_3 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_left_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_left_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_left_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_right_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_right_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_right_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_swappedSeq_m_2 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_swappedSeq_m_3 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_left_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_left_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_left_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_right_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_right_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_right_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    node _sortedArray_left_left_left_merged_swappedSeq_swapNeeded_T = lt(io_a_1, io_a_0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_left_left_merged_swappedSeq_swapNeeded = eq(_sortedArray_left_left_left_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_0 = mux(sortedArray_left_left_left_merged_swappedSeq_swapNeeded, io_a_1, io_a_0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_1 = mux(sortedArray_left_left_left_merged_swappedSeq_swapNeeded, io_a_0, io_a_1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_left_right_merged_swappedSeq_swapNeeded_T = lt(io_a_3, io_a_2) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_left_right_merged_swappedSeq_swapNeeded = eq(_sortedArray_left_left_right_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_2 = mux(sortedArray_left_left_right_merged_swappedSeq_swapNeeded, io_a_3, io_a_2) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_3 = mux(sortedArray_left_left_right_merged_swappedSeq_swapNeeded, io_a_2, io_a_3) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_left_merged_swappedSeq_swapNeeded_T = lt(sortedArray_left_left_right_merged_swappedSeq_m.io_z0, sortedArray_left_left_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_left_merged_swappedSeq_swapNeeded = eq(_sortedArray_left_left_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_4 = mux(sortedArray_left_left_merged_swappedSeq_swapNeeded, sortedArray_left_left_right_merged_swappedSeq_m.io_z0, sortedArray_left_left_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_5 = mux(sortedArray_left_left_merged_swappedSeq_swapNeeded, sortedArray_left_left_left_merged_swappedSeq_m.io_z0, sortedArray_left_left_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_left_merged_swappedSeq_swapNeeded_T_1 = lt(sortedArray_left_left_right_merged_swappedSeq_m.io_z1, sortedArray_left_left_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_left_merged_swappedSeq_swapNeeded_1 = eq(_sortedArray_left_left_merged_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_6 = mux(sortedArray_left_left_merged_swappedSeq_swapNeeded_1, sortedArray_left_left_right_merged_swappedSeq_m.io_z1, sortedArray_left_left_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_7 = mux(sortedArray_left_left_merged_swappedSeq_swapNeeded_1, sortedArray_left_left_left_merged_swappedSeq_m.io_z1, sortedArray_left_left_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_left_merged_left_swappedSeq_swapNeeded_T = lt(sortedArray_left_left_merged_swappedSeq_m_1.io_z0, sortedArray_left_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_left_merged_left_swappedSeq_swapNeeded = eq(_sortedArray_left_left_merged_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_8 = mux(sortedArray_left_left_merged_left_swappedSeq_swapNeeded, sortedArray_left_left_merged_swappedSeq_m_1.io_z0, sortedArray_left_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_9 = mux(sortedArray_left_left_merged_left_swappedSeq_swapNeeded, sortedArray_left_left_merged_swappedSeq_m.io_z0, sortedArray_left_left_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_left_merged_right_swappedSeq_swapNeeded_T = lt(sortedArray_left_left_merged_swappedSeq_m_1.io_z1, sortedArray_left_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_left_merged_right_swappedSeq_swapNeeded = eq(_sortedArray_left_left_merged_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_10 = mux(sortedArray_left_left_merged_right_swappedSeq_swapNeeded, sortedArray_left_left_merged_swappedSeq_m_1.io_z1, sortedArray_left_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_11 = mux(sortedArray_left_left_merged_right_swappedSeq_swapNeeded, sortedArray_left_left_merged_swappedSeq_m.io_z1, sortedArray_left_left_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_right_left_merged_swappedSeq_swapNeeded_T = lt(io_a_5, io_a_4) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_right_left_merged_swappedSeq_swapNeeded = eq(_sortedArray_left_right_left_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_12 = mux(sortedArray_left_right_left_merged_swappedSeq_swapNeeded, io_a_5, io_a_4) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_13 = mux(sortedArray_left_right_left_merged_swappedSeq_swapNeeded, io_a_4, io_a_5) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_right_right_merged_swappedSeq_swapNeeded_T = lt(io_a_7, io_a_6) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_right_right_merged_swappedSeq_swapNeeded = eq(_sortedArray_left_right_right_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_14 = mux(sortedArray_left_right_right_merged_swappedSeq_swapNeeded, io_a_7, io_a_6) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_15 = mux(sortedArray_left_right_right_merged_swappedSeq_swapNeeded, io_a_6, io_a_7) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_right_merged_swappedSeq_swapNeeded_T = lt(sortedArray_left_right_right_merged_swappedSeq_m.io_z0, sortedArray_left_right_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_right_merged_swappedSeq_swapNeeded = eq(_sortedArray_left_right_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_16 = mux(sortedArray_left_right_merged_swappedSeq_swapNeeded, sortedArray_left_right_right_merged_swappedSeq_m.io_z0, sortedArray_left_right_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_17 = mux(sortedArray_left_right_merged_swappedSeq_swapNeeded, sortedArray_left_right_left_merged_swappedSeq_m.io_z0, sortedArray_left_right_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_right_merged_swappedSeq_swapNeeded_T_1 = lt(sortedArray_left_right_right_merged_swappedSeq_m.io_z1, sortedArray_left_right_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_right_merged_swappedSeq_swapNeeded_1 = eq(_sortedArray_left_right_merged_swappedSeq_swapNeeded_T_1, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_18 = mux(sortedArray_left_right_merged_swappedSeq_swapNeeded_1, sortedArray_left_right_right_merged_swappedSeq_m.io_z1, sortedArray_left_right_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_19 = mux(sortedArray_left_right_merged_swappedSeq_swapNeeded_1, sortedArray_left_right_left_merged_swappedSeq_m.io_z1, sortedArray_left_right_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_right_merged_left_swappedSeq_swapNeeded_T = lt(sortedArray_left_right_merged_swappedSeq_m_1.io_z0, sortedArray_left_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_right_merged_left_swappedSeq_swapNeeded = eq(_sortedArray_left_right_merged_left_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_20 = mux(sortedArray_left_right_merged_left_swappedSeq_swapNeeded, sortedArray_left_right_merged_swappedSeq_m_1.io_z0, sortedArray_left_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_21 = mux(sortedArray_left_right_merged_left_swappedSeq_swapNeeded, sortedArray_left_right_merged_swappedSeq_m.io_z0, sortedArray_left_right_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_right_merged_right_swappedSeq_swapNeeded_T = lt(sortedArray_left_right_merged_swappedSeq_m_1.io_z1, sortedArray_left_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_right_merged_right_swappedSeq_swapNeeded = eq(_sortedArray_left_right_merged_right_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_22 = mux(sortedArray_left_right_merged_right_swappedSeq_swapNeeded, sortedArray_left_right_merged_swappedSeq_m_1.io_z1, sortedArray_left_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_23 = mux(sortedArray_left_right_merged_right_swappedSeq_swapNeeded, sortedArray_left_right_merged_swappedSeq_m.io_z1, sortedArray_left_right_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_swappedSeq_swapNeeded_T = lt(sortedArray_left_right_merged_left_swappedSeq_m.io_z0, sortedArray_left_left_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_swappedSeq_swapNeeded = eq(_sortedArray_left_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_24 = mux(sortedArray_left_merged_swappedSeq_swapNeeded, sortedArray_left_right_merged_left_swappedSeq_m.io_z0, sortedArray_left_left_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_25 = mux(sortedArray_left_merged_swappedSeq_swapNeeded, sortedArray_left_left_merged_left_swappedSeq_m.io_z0, sortedArray_left_right_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_swappedSeq_swapNeeded_T_1 = lt(sortedArray_left_right_merged_left_swappedSeq_m.io_z1, sortedArray_left_left_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_swappedSeq_swapNeeded_1 = eq(_sortedArray_left_merged_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_26 = mux(sortedArray_left_merged_swappedSeq_swapNeeded_1, sortedArray_left_right_merged_left_swappedSeq_m.io_z1, sortedArray_left_left_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_27 = mux(sortedArray_left_merged_swappedSeq_swapNeeded_1, sortedArray_left_left_merged_left_swappedSeq_m.io_z1, sortedArray_left_right_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_swappedSeq_swapNeeded_T_2 = lt(sortedArray_left_right_merged_right_swappedSeq_m.io_z0, sortedArray_left_left_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_swappedSeq_swapNeeded_2 = eq(_sortedArray_left_merged_swappedSeq_swapNeeded_T_2, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_28 = mux(sortedArray_left_merged_swappedSeq_swapNeeded_2, sortedArray_left_right_merged_right_swappedSeq_m.io_z0, sortedArray_left_left_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_29 = mux(sortedArray_left_merged_swappedSeq_swapNeeded_2, sortedArray_left_left_merged_right_swappedSeq_m.io_z0, sortedArray_left_right_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_swappedSeq_swapNeeded_T_3 = lt(sortedArray_left_right_merged_right_swappedSeq_m.io_z1, sortedArray_left_left_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_swappedSeq_swapNeeded_3 = eq(_sortedArray_left_merged_swappedSeq_swapNeeded_T_3, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_30 = mux(sortedArray_left_merged_swappedSeq_swapNeeded_3, sortedArray_left_right_merged_right_swappedSeq_m.io_z1, sortedArray_left_left_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_31 = mux(sortedArray_left_merged_swappedSeq_swapNeeded_3, sortedArray_left_left_merged_right_swappedSeq_m.io_z1, sortedArray_left_right_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_left_swappedSeq_swapNeeded_T = lt(sortedArray_left_merged_swappedSeq_m_2.io_z0, sortedArray_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_left_swappedSeq_swapNeeded = eq(_sortedArray_left_merged_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_32 = mux(sortedArray_left_merged_left_swappedSeq_swapNeeded, sortedArray_left_merged_swappedSeq_m_2.io_z0, sortedArray_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_33 = mux(sortedArray_left_merged_left_swappedSeq_swapNeeded, sortedArray_left_merged_swappedSeq_m.io_z0, sortedArray_left_merged_swappedSeq_m_2.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_left_swappedSeq_swapNeeded_T_1 = lt(sortedArray_left_merged_swappedSeq_m_3.io_z0, sortedArray_left_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_left_swappedSeq_swapNeeded_1 = eq(_sortedArray_left_merged_left_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_34 = mux(sortedArray_left_merged_left_swappedSeq_swapNeeded_1, sortedArray_left_merged_swappedSeq_m_3.io_z0, sortedArray_left_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_35 = mux(sortedArray_left_merged_left_swappedSeq_swapNeeded_1, sortedArray_left_merged_swappedSeq_m_1.io_z0, sortedArray_left_merged_swappedSeq_m_3.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_left_left_swappedSeq_swapNeeded_T = lt(sortedArray_left_merged_left_swappedSeq_m_1.io_z0, sortedArray_left_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_left_left_swappedSeq_swapNeeded = eq(_sortedArray_left_merged_left_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_36 = mux(sortedArray_left_merged_left_left_swappedSeq_swapNeeded, sortedArray_left_merged_left_swappedSeq_m_1.io_z0, sortedArray_left_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_37 = mux(sortedArray_left_merged_left_left_swappedSeq_swapNeeded, sortedArray_left_merged_left_swappedSeq_m.io_z0, sortedArray_left_merged_left_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_left_right_swappedSeq_swapNeeded_T = lt(sortedArray_left_merged_left_swappedSeq_m_1.io_z1, sortedArray_left_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_left_right_swappedSeq_swapNeeded = eq(_sortedArray_left_merged_left_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_38 = mux(sortedArray_left_merged_left_right_swappedSeq_swapNeeded, sortedArray_left_merged_left_swappedSeq_m_1.io_z1, sortedArray_left_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_39 = mux(sortedArray_left_merged_left_right_swappedSeq_swapNeeded, sortedArray_left_merged_left_swappedSeq_m.io_z1, sortedArray_left_merged_left_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_right_swappedSeq_swapNeeded_T = lt(sortedArray_left_merged_swappedSeq_m_2.io_z1, sortedArray_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_right_swappedSeq_swapNeeded = eq(_sortedArray_left_merged_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_40 = mux(sortedArray_left_merged_right_swappedSeq_swapNeeded, sortedArray_left_merged_swappedSeq_m_2.io_z1, sortedArray_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_41 = mux(sortedArray_left_merged_right_swappedSeq_swapNeeded, sortedArray_left_merged_swappedSeq_m.io_z1, sortedArray_left_merged_swappedSeq_m_2.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_right_swappedSeq_swapNeeded_T_1 = lt(sortedArray_left_merged_swappedSeq_m_3.io_z1, sortedArray_left_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_right_swappedSeq_swapNeeded_1 = eq(_sortedArray_left_merged_right_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_42 = mux(sortedArray_left_merged_right_swappedSeq_swapNeeded_1, sortedArray_left_merged_swappedSeq_m_3.io_z1, sortedArray_left_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_43 = mux(sortedArray_left_merged_right_swappedSeq_swapNeeded_1, sortedArray_left_merged_swappedSeq_m_1.io_z1, sortedArray_left_merged_swappedSeq_m_3.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_right_left_swappedSeq_swapNeeded_T = lt(sortedArray_left_merged_right_swappedSeq_m_1.io_z0, sortedArray_left_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_right_left_swappedSeq_swapNeeded = eq(_sortedArray_left_merged_right_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_44 = mux(sortedArray_left_merged_right_left_swappedSeq_swapNeeded, sortedArray_left_merged_right_swappedSeq_m_1.io_z0, sortedArray_left_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_45 = mux(sortedArray_left_merged_right_left_swappedSeq_swapNeeded, sortedArray_left_merged_right_swappedSeq_m.io_z0, sortedArray_left_merged_right_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_right_right_swappedSeq_swapNeeded_T = lt(sortedArray_left_merged_right_swappedSeq_m_1.io_z1, sortedArray_left_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_left_merged_right_right_swappedSeq_swapNeeded = eq(_sortedArray_left_merged_right_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_46 = mux(sortedArray_left_merged_right_right_swappedSeq_swapNeeded, sortedArray_left_merged_right_swappedSeq_m_1.io_z1, sortedArray_left_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_47 = mux(sortedArray_left_merged_right_right_swappedSeq_swapNeeded, sortedArray_left_merged_right_swappedSeq_m.io_z1, sortedArray_left_merged_right_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_left_left_merged_swappedSeq_swapNeeded_T = lt(io_a_9, io_a_8) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_left_left_merged_swappedSeq_swapNeeded = eq(_sortedArray_right_left_left_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_48 = mux(sortedArray_right_left_left_merged_swappedSeq_swapNeeded, io_a_9, io_a_8) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_49 = mux(sortedArray_right_left_left_merged_swappedSeq_swapNeeded, io_a_8, io_a_9) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_left_right_merged_swappedSeq_swapNeeded_T = lt(io_a_11, io_a_10) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_left_right_merged_swappedSeq_swapNeeded = eq(_sortedArray_right_left_right_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_50 = mux(sortedArray_right_left_right_merged_swappedSeq_swapNeeded, io_a_11, io_a_10) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_51 = mux(sortedArray_right_left_right_merged_swappedSeq_swapNeeded, io_a_10, io_a_11) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_left_merged_swappedSeq_swapNeeded_T = lt(sortedArray_right_left_right_merged_swappedSeq_m.io_z0, sortedArray_right_left_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_left_merged_swappedSeq_swapNeeded = eq(_sortedArray_right_left_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_52 = mux(sortedArray_right_left_merged_swappedSeq_swapNeeded, sortedArray_right_left_right_merged_swappedSeq_m.io_z0, sortedArray_right_left_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_53 = mux(sortedArray_right_left_merged_swappedSeq_swapNeeded, sortedArray_right_left_left_merged_swappedSeq_m.io_z0, sortedArray_right_left_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_left_merged_swappedSeq_swapNeeded_T_1 = lt(sortedArray_right_left_right_merged_swappedSeq_m.io_z1, sortedArray_right_left_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_left_merged_swappedSeq_swapNeeded_1 = eq(_sortedArray_right_left_merged_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_54 = mux(sortedArray_right_left_merged_swappedSeq_swapNeeded_1, sortedArray_right_left_right_merged_swappedSeq_m.io_z1, sortedArray_right_left_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_55 = mux(sortedArray_right_left_merged_swappedSeq_swapNeeded_1, sortedArray_right_left_left_merged_swappedSeq_m.io_z1, sortedArray_right_left_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_left_merged_left_swappedSeq_swapNeeded_T = lt(sortedArray_right_left_merged_swappedSeq_m_1.io_z0, sortedArray_right_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_left_merged_left_swappedSeq_swapNeeded = eq(_sortedArray_right_left_merged_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_56 = mux(sortedArray_right_left_merged_left_swappedSeq_swapNeeded, sortedArray_right_left_merged_swappedSeq_m_1.io_z0, sortedArray_right_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_57 = mux(sortedArray_right_left_merged_left_swappedSeq_swapNeeded, sortedArray_right_left_merged_swappedSeq_m.io_z0, sortedArray_right_left_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_left_merged_right_swappedSeq_swapNeeded_T = lt(sortedArray_right_left_merged_swappedSeq_m_1.io_z1, sortedArray_right_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_left_merged_right_swappedSeq_swapNeeded = eq(_sortedArray_right_left_merged_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_58 = mux(sortedArray_right_left_merged_right_swappedSeq_swapNeeded, sortedArray_right_left_merged_swappedSeq_m_1.io_z1, sortedArray_right_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_59 = mux(sortedArray_right_left_merged_right_swappedSeq_swapNeeded, sortedArray_right_left_merged_swappedSeq_m.io_z1, sortedArray_right_left_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_right_left_merged_swappedSeq_swapNeeded_T = lt(io_a_13, io_a_12) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_right_left_merged_swappedSeq_swapNeeded = eq(_sortedArray_right_right_left_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_60 = mux(sortedArray_right_right_left_merged_swappedSeq_swapNeeded, io_a_13, io_a_12) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_61 = mux(sortedArray_right_right_left_merged_swappedSeq_swapNeeded, io_a_12, io_a_13) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_right_right_merged_swappedSeq_swapNeeded_T = lt(io_a_15, io_a_14) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_right_right_merged_swappedSeq_swapNeeded = eq(_sortedArray_right_right_right_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_62 = mux(sortedArray_right_right_right_merged_swappedSeq_swapNeeded, io_a_15, io_a_14) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_63 = mux(sortedArray_right_right_right_merged_swappedSeq_swapNeeded, io_a_14, io_a_15) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_right_merged_swappedSeq_swapNeeded_T = lt(sortedArray_right_right_right_merged_swappedSeq_m.io_z0, sortedArray_right_right_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_right_merged_swappedSeq_swapNeeded = eq(_sortedArray_right_right_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_64 = mux(sortedArray_right_right_merged_swappedSeq_swapNeeded, sortedArray_right_right_right_merged_swappedSeq_m.io_z0, sortedArray_right_right_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_65 = mux(sortedArray_right_right_merged_swappedSeq_swapNeeded, sortedArray_right_right_left_merged_swappedSeq_m.io_z0, sortedArray_right_right_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_right_merged_swappedSeq_swapNeeded_T_1 = lt(sortedArray_right_right_right_merged_swappedSeq_m.io_z1, sortedArray_right_right_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_right_merged_swappedSeq_swapNeeded_1 = eq(_sortedArray_right_right_merged_swappedSeq_swapNeeded_T_1, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_66 = mux(sortedArray_right_right_merged_swappedSeq_swapNeeded_1, sortedArray_right_right_right_merged_swappedSeq_m.io_z1, sortedArray_right_right_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_67 = mux(sortedArray_right_right_merged_swappedSeq_swapNeeded_1, sortedArray_right_right_left_merged_swappedSeq_m.io_z1, sortedArray_right_right_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_right_merged_left_swappedSeq_swapNeeded_T = lt(sortedArray_right_right_merged_swappedSeq_m_1.io_z0, sortedArray_right_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_right_merged_left_swappedSeq_swapNeeded = eq(_sortedArray_right_right_merged_left_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_68 = mux(sortedArray_right_right_merged_left_swappedSeq_swapNeeded, sortedArray_right_right_merged_swappedSeq_m_1.io_z0, sortedArray_right_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_69 = mux(sortedArray_right_right_merged_left_swappedSeq_swapNeeded, sortedArray_right_right_merged_swappedSeq_m.io_z0, sortedArray_right_right_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_right_merged_right_swappedSeq_swapNeeded_T = lt(sortedArray_right_right_merged_swappedSeq_m_1.io_z1, sortedArray_right_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_right_merged_right_swappedSeq_swapNeeded = eq(_sortedArray_right_right_merged_right_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_70 = mux(sortedArray_right_right_merged_right_swappedSeq_swapNeeded, sortedArray_right_right_merged_swappedSeq_m_1.io_z1, sortedArray_right_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_71 = mux(sortedArray_right_right_merged_right_swappedSeq_swapNeeded, sortedArray_right_right_merged_swappedSeq_m.io_z1, sortedArray_right_right_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_swappedSeq_swapNeeded_T = lt(sortedArray_right_right_merged_left_swappedSeq_m.io_z0, sortedArray_right_left_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_swappedSeq_swapNeeded = eq(_sortedArray_right_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_72 = mux(sortedArray_right_merged_swappedSeq_swapNeeded, sortedArray_right_right_merged_left_swappedSeq_m.io_z0, sortedArray_right_left_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_73 = mux(sortedArray_right_merged_swappedSeq_swapNeeded, sortedArray_right_left_merged_left_swappedSeq_m.io_z0, sortedArray_right_right_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_swappedSeq_swapNeeded_T_1 = lt(sortedArray_right_right_merged_left_swappedSeq_m.io_z1, sortedArray_right_left_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_swappedSeq_swapNeeded_1 = eq(_sortedArray_right_merged_swappedSeq_swapNeeded_T_1, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_74 = mux(sortedArray_right_merged_swappedSeq_swapNeeded_1, sortedArray_right_right_merged_left_swappedSeq_m.io_z1, sortedArray_right_left_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_75 = mux(sortedArray_right_merged_swappedSeq_swapNeeded_1, sortedArray_right_left_merged_left_swappedSeq_m.io_z1, sortedArray_right_right_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_swappedSeq_swapNeeded_T_2 = lt(sortedArray_right_right_merged_right_swappedSeq_m.io_z0, sortedArray_right_left_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_swappedSeq_swapNeeded_2 = eq(_sortedArray_right_merged_swappedSeq_swapNeeded_T_2, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_76 = mux(sortedArray_right_merged_swappedSeq_swapNeeded_2, sortedArray_right_right_merged_right_swappedSeq_m.io_z0, sortedArray_right_left_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_77 = mux(sortedArray_right_merged_swappedSeq_swapNeeded_2, sortedArray_right_left_merged_right_swappedSeq_m.io_z0, sortedArray_right_right_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_swappedSeq_swapNeeded_T_3 = lt(sortedArray_right_right_merged_right_swappedSeq_m.io_z1, sortedArray_right_left_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_swappedSeq_swapNeeded_3 = eq(_sortedArray_right_merged_swappedSeq_swapNeeded_T_3, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_78 = mux(sortedArray_right_merged_swappedSeq_swapNeeded_3, sortedArray_right_right_merged_right_swappedSeq_m.io_z1, sortedArray_right_left_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_79 = mux(sortedArray_right_merged_swappedSeq_swapNeeded_3, sortedArray_right_left_merged_right_swappedSeq_m.io_z1, sortedArray_right_right_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_left_swappedSeq_swapNeeded_T = lt(sortedArray_right_merged_swappedSeq_m_2.io_z0, sortedArray_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_left_swappedSeq_swapNeeded = eq(_sortedArray_right_merged_left_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_80 = mux(sortedArray_right_merged_left_swappedSeq_swapNeeded, sortedArray_right_merged_swappedSeq_m_2.io_z0, sortedArray_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_81 = mux(sortedArray_right_merged_left_swappedSeq_swapNeeded, sortedArray_right_merged_swappedSeq_m.io_z0, sortedArray_right_merged_swappedSeq_m_2.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_left_swappedSeq_swapNeeded_T_1 = lt(sortedArray_right_merged_swappedSeq_m_3.io_z0, sortedArray_right_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_left_swappedSeq_swapNeeded_1 = eq(_sortedArray_right_merged_left_swappedSeq_swapNeeded_T_1, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_82 = mux(sortedArray_right_merged_left_swappedSeq_swapNeeded_1, sortedArray_right_merged_swappedSeq_m_3.io_z0, sortedArray_right_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_83 = mux(sortedArray_right_merged_left_swappedSeq_swapNeeded_1, sortedArray_right_merged_swappedSeq_m_1.io_z0, sortedArray_right_merged_swappedSeq_m_3.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_left_left_swappedSeq_swapNeeded_T = lt(sortedArray_right_merged_left_swappedSeq_m_1.io_z0, sortedArray_right_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_left_left_swappedSeq_swapNeeded = eq(_sortedArray_right_merged_left_left_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_84 = mux(sortedArray_right_merged_left_left_swappedSeq_swapNeeded, sortedArray_right_merged_left_swappedSeq_m_1.io_z0, sortedArray_right_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_85 = mux(sortedArray_right_merged_left_left_swappedSeq_swapNeeded, sortedArray_right_merged_left_swappedSeq_m.io_z0, sortedArray_right_merged_left_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_left_right_swappedSeq_swapNeeded_T = lt(sortedArray_right_merged_left_swappedSeq_m_1.io_z1, sortedArray_right_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_left_right_swappedSeq_swapNeeded = eq(_sortedArray_right_merged_left_right_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_86 = mux(sortedArray_right_merged_left_right_swappedSeq_swapNeeded, sortedArray_right_merged_left_swappedSeq_m_1.io_z1, sortedArray_right_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_87 = mux(sortedArray_right_merged_left_right_swappedSeq_swapNeeded, sortedArray_right_merged_left_swappedSeq_m.io_z1, sortedArray_right_merged_left_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_right_swappedSeq_swapNeeded_T = lt(sortedArray_right_merged_swappedSeq_m_2.io_z1, sortedArray_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_right_swappedSeq_swapNeeded = eq(_sortedArray_right_merged_right_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_88 = mux(sortedArray_right_merged_right_swappedSeq_swapNeeded, sortedArray_right_merged_swappedSeq_m_2.io_z1, sortedArray_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_89 = mux(sortedArray_right_merged_right_swappedSeq_swapNeeded, sortedArray_right_merged_swappedSeq_m.io_z1, sortedArray_right_merged_swappedSeq_m_2.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_right_swappedSeq_swapNeeded_T_1 = lt(sortedArray_right_merged_swappedSeq_m_3.io_z1, sortedArray_right_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_right_swappedSeq_swapNeeded_1 = eq(_sortedArray_right_merged_right_swappedSeq_swapNeeded_T_1, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_90 = mux(sortedArray_right_merged_right_swappedSeq_swapNeeded_1, sortedArray_right_merged_swappedSeq_m_3.io_z1, sortedArray_right_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_91 = mux(sortedArray_right_merged_right_swappedSeq_swapNeeded_1, sortedArray_right_merged_swappedSeq_m_1.io_z1, sortedArray_right_merged_swappedSeq_m_3.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_right_left_swappedSeq_swapNeeded_T = lt(sortedArray_right_merged_right_swappedSeq_m_1.io_z0, sortedArray_right_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_right_left_swappedSeq_swapNeeded = eq(_sortedArray_right_merged_right_left_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_92 = mux(sortedArray_right_merged_right_left_swappedSeq_swapNeeded, sortedArray_right_merged_right_swappedSeq_m_1.io_z0, sortedArray_right_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_93 = mux(sortedArray_right_merged_right_left_swappedSeq_swapNeeded, sortedArray_right_merged_right_swappedSeq_m.io_z0, sortedArray_right_merged_right_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_right_right_swappedSeq_swapNeeded_T = lt(sortedArray_right_merged_right_swappedSeq_m_1.io_z1, sortedArray_right_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_right_merged_right_right_swappedSeq_swapNeeded = eq(_sortedArray_right_merged_right_right_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_94 = mux(sortedArray_right_merged_right_right_swappedSeq_swapNeeded, sortedArray_right_merged_right_swappedSeq_m_1.io_z1, sortedArray_right_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_95 = mux(sortedArray_right_merged_right_right_swappedSeq_swapNeeded, sortedArray_right_merged_right_swappedSeq_m.io_z1, sortedArray_right_merged_right_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T = lt(sortedArray_right_merged_left_left_swappedSeq_m.io_z0, sortedArray_left_merged_left_left_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_swappedSeq_swapNeeded = eq(_sortedArray_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_96 = mux(sortedArray_merged_swappedSeq_swapNeeded, sortedArray_right_merged_left_left_swappedSeq_m.io_z0, sortedArray_left_merged_left_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_97 = mux(sortedArray_merged_swappedSeq_swapNeeded, sortedArray_left_merged_left_left_swappedSeq_m.io_z0, sortedArray_right_merged_left_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T_1 = lt(sortedArray_right_merged_left_left_swappedSeq_m.io_z1, sortedArray_left_merged_left_left_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_swappedSeq_swapNeeded_1 = eq(_sortedArray_merged_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_98 = mux(sortedArray_merged_swappedSeq_swapNeeded_1, sortedArray_right_merged_left_left_swappedSeq_m.io_z1, sortedArray_left_merged_left_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_99 = mux(sortedArray_merged_swappedSeq_swapNeeded_1, sortedArray_left_merged_left_left_swappedSeq_m.io_z1, sortedArray_right_merged_left_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T_2 = lt(sortedArray_right_merged_left_right_swappedSeq_m.io_z0, sortedArray_left_merged_left_right_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_swappedSeq_swapNeeded_2 = eq(_sortedArray_merged_swappedSeq_swapNeeded_T_2, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_100 = mux(sortedArray_merged_swappedSeq_swapNeeded_2, sortedArray_right_merged_left_right_swappedSeq_m.io_z0, sortedArray_left_merged_left_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_101 = mux(sortedArray_merged_swappedSeq_swapNeeded_2, sortedArray_left_merged_left_right_swappedSeq_m.io_z0, sortedArray_right_merged_left_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T_3 = lt(sortedArray_right_merged_left_right_swappedSeq_m.io_z1, sortedArray_left_merged_left_right_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_swappedSeq_swapNeeded_3 = eq(_sortedArray_merged_swappedSeq_swapNeeded_T_3, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_102 = mux(sortedArray_merged_swappedSeq_swapNeeded_3, sortedArray_right_merged_left_right_swappedSeq_m.io_z1, sortedArray_left_merged_left_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_103 = mux(sortedArray_merged_swappedSeq_swapNeeded_3, sortedArray_left_merged_left_right_swappedSeq_m.io_z1, sortedArray_right_merged_left_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T_4 = lt(sortedArray_right_merged_right_left_swappedSeq_m.io_z0, sortedArray_left_merged_right_left_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_swappedSeq_swapNeeded_4 = eq(_sortedArray_merged_swappedSeq_swapNeeded_T_4, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_104 = mux(sortedArray_merged_swappedSeq_swapNeeded_4, sortedArray_right_merged_right_left_swappedSeq_m.io_z0, sortedArray_left_merged_right_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_105 = mux(sortedArray_merged_swappedSeq_swapNeeded_4, sortedArray_left_merged_right_left_swappedSeq_m.io_z0, sortedArray_right_merged_right_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T_5 = lt(sortedArray_right_merged_right_left_swappedSeq_m.io_z1, sortedArray_left_merged_right_left_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_swappedSeq_swapNeeded_5 = eq(_sortedArray_merged_swappedSeq_swapNeeded_T_5, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_106 = mux(sortedArray_merged_swappedSeq_swapNeeded_5, sortedArray_right_merged_right_left_swappedSeq_m.io_z1, sortedArray_left_merged_right_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_107 = mux(sortedArray_merged_swappedSeq_swapNeeded_5, sortedArray_left_merged_right_left_swappedSeq_m.io_z1, sortedArray_right_merged_right_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T_6 = lt(sortedArray_right_merged_right_right_swappedSeq_m.io_z0, sortedArray_left_merged_right_right_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_swappedSeq_swapNeeded_6 = eq(_sortedArray_merged_swappedSeq_swapNeeded_T_6, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_108 = mux(sortedArray_merged_swappedSeq_swapNeeded_6, sortedArray_right_merged_right_right_swappedSeq_m.io_z0, sortedArray_left_merged_right_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_109 = mux(sortedArray_merged_swappedSeq_swapNeeded_6, sortedArray_left_merged_right_right_swappedSeq_m.io_z0, sortedArray_right_merged_right_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T_7 = lt(sortedArray_right_merged_right_right_swappedSeq_m.io_z1, sortedArray_left_merged_right_right_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_swappedSeq_swapNeeded_7 = eq(_sortedArray_merged_swappedSeq_swapNeeded_T_7, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_110 = mux(sortedArray_merged_swappedSeq_swapNeeded_7, sortedArray_right_merged_right_right_swappedSeq_m.io_z1, sortedArray_left_merged_right_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_111 = mux(sortedArray_merged_swappedSeq_swapNeeded_7, sortedArray_left_merged_right_right_swappedSeq_m.io_z1, sortedArray_right_merged_right_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_swappedSeq_swapNeeded_T = lt(sortedArray_merged_swappedSeq_m_4.io_z0, sortedArray_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_swappedSeq_swapNeeded = eq(_sortedArray_merged_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_112 = mux(sortedArray_merged_left_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m_4.io_z0, sortedArray_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_113 = mux(sortedArray_merged_left_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m.io_z0, sortedArray_merged_swappedSeq_m_4.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_swappedSeq_swapNeeded_T_1 = lt(sortedArray_merged_swappedSeq_m_5.io_z0, sortedArray_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_swappedSeq_swapNeeded_1 = eq(_sortedArray_merged_left_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_114 = mux(sortedArray_merged_left_swappedSeq_swapNeeded_1, sortedArray_merged_swappedSeq_m_5.io_z0, sortedArray_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_115 = mux(sortedArray_merged_left_swappedSeq_swapNeeded_1, sortedArray_merged_swappedSeq_m_1.io_z0, sortedArray_merged_swappedSeq_m_5.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_swappedSeq_swapNeeded_T_2 = lt(sortedArray_merged_swappedSeq_m_6.io_z0, sortedArray_merged_swappedSeq_m_2.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_swappedSeq_swapNeeded_2 = eq(_sortedArray_merged_left_swappedSeq_swapNeeded_T_2, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_116 = mux(sortedArray_merged_left_swappedSeq_swapNeeded_2, sortedArray_merged_swappedSeq_m_6.io_z0, sortedArray_merged_swappedSeq_m_2.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_117 = mux(sortedArray_merged_left_swappedSeq_swapNeeded_2, sortedArray_merged_swappedSeq_m_2.io_z0, sortedArray_merged_swappedSeq_m_6.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_swappedSeq_swapNeeded_T_3 = lt(sortedArray_merged_swappedSeq_m_7.io_z0, sortedArray_merged_swappedSeq_m_3.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_swappedSeq_swapNeeded_3 = eq(_sortedArray_merged_left_swappedSeq_swapNeeded_T_3, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_118 = mux(sortedArray_merged_left_swappedSeq_swapNeeded_3, sortedArray_merged_swappedSeq_m_7.io_z0, sortedArray_merged_swappedSeq_m_3.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_119 = mux(sortedArray_merged_left_swappedSeq_swapNeeded_3, sortedArray_merged_swappedSeq_m_3.io_z0, sortedArray_merged_swappedSeq_m_7.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_left_swappedSeq_swapNeeded_T = lt(sortedArray_merged_left_swappedSeq_m_2.io_z0, sortedArray_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_left_swappedSeq_swapNeeded = eq(_sortedArray_merged_left_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_120 = mux(sortedArray_merged_left_left_swappedSeq_swapNeeded, sortedArray_merged_left_swappedSeq_m_2.io_z0, sortedArray_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_121 = mux(sortedArray_merged_left_left_swappedSeq_swapNeeded, sortedArray_merged_left_swappedSeq_m.io_z0, sortedArray_merged_left_swappedSeq_m_2.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_left_swappedSeq_swapNeeded_T_1 = lt(sortedArray_merged_left_swappedSeq_m_3.io_z0, sortedArray_merged_left_swappedSeq_m_1.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_left_swappedSeq_swapNeeded_1 = eq(_sortedArray_merged_left_left_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_122 = mux(sortedArray_merged_left_left_swappedSeq_swapNeeded_1, sortedArray_merged_left_swappedSeq_m_3.io_z0, sortedArray_merged_left_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_123 = mux(sortedArray_merged_left_left_swappedSeq_swapNeeded_1, sortedArray_merged_left_swappedSeq_m_1.io_z0, sortedArray_merged_left_swappedSeq_m_3.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_left_left_swappedSeq_swapNeeded_T = lt(sortedArray_merged_left_left_swappedSeq_m_1.io_z0, sortedArray_merged_left_left_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_left_left_swappedSeq_swapNeeded = eq(_sortedArray_merged_left_left_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_124 = mux(sortedArray_merged_left_left_left_swappedSeq_swapNeeded, sortedArray_merged_left_left_swappedSeq_m_1.io_z0, sortedArray_merged_left_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_125 = mux(sortedArray_merged_left_left_left_swappedSeq_swapNeeded, sortedArray_merged_left_left_swappedSeq_m.io_z0, sortedArray_merged_left_left_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_left_right_swappedSeq_swapNeeded_T = lt(sortedArray_merged_left_left_swappedSeq_m_1.io_z1, sortedArray_merged_left_left_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_left_right_swappedSeq_swapNeeded = eq(_sortedArray_merged_left_left_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_126 = mux(sortedArray_merged_left_left_right_swappedSeq_swapNeeded, sortedArray_merged_left_left_swappedSeq_m_1.io_z1, sortedArray_merged_left_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_127 = mux(sortedArray_merged_left_left_right_swappedSeq_swapNeeded, sortedArray_merged_left_left_swappedSeq_m.io_z1, sortedArray_merged_left_left_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_right_swappedSeq_swapNeeded_T = lt(sortedArray_merged_left_swappedSeq_m_2.io_z1, sortedArray_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_right_swappedSeq_swapNeeded = eq(_sortedArray_merged_left_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_128 = mux(sortedArray_merged_left_right_swappedSeq_swapNeeded, sortedArray_merged_left_swappedSeq_m_2.io_z1, sortedArray_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_129 = mux(sortedArray_merged_left_right_swappedSeq_swapNeeded, sortedArray_merged_left_swappedSeq_m.io_z1, sortedArray_merged_left_swappedSeq_m_2.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_right_swappedSeq_swapNeeded_T_1 = lt(sortedArray_merged_left_swappedSeq_m_3.io_z1, sortedArray_merged_left_swappedSeq_m_1.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_right_swappedSeq_swapNeeded_1 = eq(_sortedArray_merged_left_right_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_130 = mux(sortedArray_merged_left_right_swappedSeq_swapNeeded_1, sortedArray_merged_left_swappedSeq_m_3.io_z1, sortedArray_merged_left_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_131 = mux(sortedArray_merged_left_right_swappedSeq_swapNeeded_1, sortedArray_merged_left_swappedSeq_m_1.io_z1, sortedArray_merged_left_swappedSeq_m_3.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_right_left_swappedSeq_swapNeeded_T = lt(sortedArray_merged_left_right_swappedSeq_m_1.io_z0, sortedArray_merged_left_right_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_right_left_swappedSeq_swapNeeded = eq(_sortedArray_merged_left_right_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_132 = mux(sortedArray_merged_left_right_left_swappedSeq_swapNeeded, sortedArray_merged_left_right_swappedSeq_m_1.io_z0, sortedArray_merged_left_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_133 = mux(sortedArray_merged_left_right_left_swappedSeq_swapNeeded, sortedArray_merged_left_right_swappedSeq_m.io_z0, sortedArray_merged_left_right_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_right_right_swappedSeq_swapNeeded_T = lt(sortedArray_merged_left_right_swappedSeq_m_1.io_z1, sortedArray_merged_left_right_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_left_right_right_swappedSeq_swapNeeded = eq(_sortedArray_merged_left_right_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_134 = mux(sortedArray_merged_left_right_right_swappedSeq_swapNeeded, sortedArray_merged_left_right_swappedSeq_m_1.io_z1, sortedArray_merged_left_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_135 = mux(sortedArray_merged_left_right_right_swappedSeq_swapNeeded, sortedArray_merged_left_right_swappedSeq_m.io_z1, sortedArray_merged_left_right_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_swappedSeq_swapNeeded_T = lt(sortedArray_merged_swappedSeq_m_4.io_z1, sortedArray_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_swappedSeq_swapNeeded = eq(_sortedArray_merged_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_136 = mux(sortedArray_merged_right_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m_4.io_z1, sortedArray_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_137 = mux(sortedArray_merged_right_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m.io_z1, sortedArray_merged_swappedSeq_m_4.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_swappedSeq_swapNeeded_T_1 = lt(sortedArray_merged_swappedSeq_m_5.io_z1, sortedArray_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_swappedSeq_swapNeeded_1 = eq(_sortedArray_merged_right_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_138 = mux(sortedArray_merged_right_swappedSeq_swapNeeded_1, sortedArray_merged_swappedSeq_m_5.io_z1, sortedArray_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_139 = mux(sortedArray_merged_right_swappedSeq_swapNeeded_1, sortedArray_merged_swappedSeq_m_1.io_z1, sortedArray_merged_swappedSeq_m_5.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_swappedSeq_swapNeeded_T_2 = lt(sortedArray_merged_swappedSeq_m_6.io_z1, sortedArray_merged_swappedSeq_m_2.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_swappedSeq_swapNeeded_2 = eq(_sortedArray_merged_right_swappedSeq_swapNeeded_T_2, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_140 = mux(sortedArray_merged_right_swappedSeq_swapNeeded_2, sortedArray_merged_swappedSeq_m_6.io_z1, sortedArray_merged_swappedSeq_m_2.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_141 = mux(sortedArray_merged_right_swappedSeq_swapNeeded_2, sortedArray_merged_swappedSeq_m_2.io_z1, sortedArray_merged_swappedSeq_m_6.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_swappedSeq_swapNeeded_T_3 = lt(sortedArray_merged_swappedSeq_m_7.io_z1, sortedArray_merged_swappedSeq_m_3.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_swappedSeq_swapNeeded_3 = eq(_sortedArray_merged_right_swappedSeq_swapNeeded_T_3, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_142 = mux(sortedArray_merged_right_swappedSeq_swapNeeded_3, sortedArray_merged_swappedSeq_m_7.io_z1, sortedArray_merged_swappedSeq_m_3.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_143 = mux(sortedArray_merged_right_swappedSeq_swapNeeded_3, sortedArray_merged_swappedSeq_m_3.io_z1, sortedArray_merged_swappedSeq_m_7.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_left_swappedSeq_swapNeeded_T = lt(sortedArray_merged_right_swappedSeq_m_2.io_z0, sortedArray_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_left_swappedSeq_swapNeeded = eq(_sortedArray_merged_right_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_144 = mux(sortedArray_merged_right_left_swappedSeq_swapNeeded, sortedArray_merged_right_swappedSeq_m_2.io_z0, sortedArray_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_145 = mux(sortedArray_merged_right_left_swappedSeq_swapNeeded, sortedArray_merged_right_swappedSeq_m.io_z0, sortedArray_merged_right_swappedSeq_m_2.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_left_swappedSeq_swapNeeded_T_1 = lt(sortedArray_merged_right_swappedSeq_m_3.io_z0, sortedArray_merged_right_swappedSeq_m_1.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_left_swappedSeq_swapNeeded_1 = eq(_sortedArray_merged_right_left_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_146 = mux(sortedArray_merged_right_left_swappedSeq_swapNeeded_1, sortedArray_merged_right_swappedSeq_m_3.io_z0, sortedArray_merged_right_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_147 = mux(sortedArray_merged_right_left_swappedSeq_swapNeeded_1, sortedArray_merged_right_swappedSeq_m_1.io_z0, sortedArray_merged_right_swappedSeq_m_3.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_left_left_swappedSeq_swapNeeded_T = lt(sortedArray_merged_right_left_swappedSeq_m_1.io_z0, sortedArray_merged_right_left_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_left_left_swappedSeq_swapNeeded = eq(_sortedArray_merged_right_left_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_148 = mux(sortedArray_merged_right_left_left_swappedSeq_swapNeeded, sortedArray_merged_right_left_swappedSeq_m_1.io_z0, sortedArray_merged_right_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_149 = mux(sortedArray_merged_right_left_left_swappedSeq_swapNeeded, sortedArray_merged_right_left_swappedSeq_m.io_z0, sortedArray_merged_right_left_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_left_right_swappedSeq_swapNeeded_T = lt(sortedArray_merged_right_left_swappedSeq_m_1.io_z1, sortedArray_merged_right_left_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_left_right_swappedSeq_swapNeeded = eq(_sortedArray_merged_right_left_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_150 = mux(sortedArray_merged_right_left_right_swappedSeq_swapNeeded, sortedArray_merged_right_left_swappedSeq_m_1.io_z1, sortedArray_merged_right_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_151 = mux(sortedArray_merged_right_left_right_swappedSeq_swapNeeded, sortedArray_merged_right_left_swappedSeq_m.io_z1, sortedArray_merged_right_left_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_right_swappedSeq_swapNeeded_T = lt(sortedArray_merged_right_swappedSeq_m_2.io_z1, sortedArray_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_right_swappedSeq_swapNeeded = eq(_sortedArray_merged_right_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_152 = mux(sortedArray_merged_right_right_swappedSeq_swapNeeded, sortedArray_merged_right_swappedSeq_m_2.io_z1, sortedArray_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_153 = mux(sortedArray_merged_right_right_swappedSeq_swapNeeded, sortedArray_merged_right_swappedSeq_m.io_z1, sortedArray_merged_right_swappedSeq_m_2.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_right_swappedSeq_swapNeeded_T_1 = lt(sortedArray_merged_right_swappedSeq_m_3.io_z1, sortedArray_merged_right_swappedSeq_m_1.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_right_swappedSeq_swapNeeded_1 = eq(_sortedArray_merged_right_right_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_154 = mux(sortedArray_merged_right_right_swappedSeq_swapNeeded_1, sortedArray_merged_right_swappedSeq_m_3.io_z1, sortedArray_merged_right_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_155 = mux(sortedArray_merged_right_right_swappedSeq_swapNeeded_1, sortedArray_merged_right_swappedSeq_m_1.io_z1, sortedArray_merged_right_swappedSeq_m_3.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_right_left_swappedSeq_swapNeeded_T = lt(sortedArray_merged_right_right_swappedSeq_m_1.io_z0, sortedArray_merged_right_right_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_right_left_swappedSeq_swapNeeded = eq(_sortedArray_merged_right_right_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_156 = mux(sortedArray_merged_right_right_left_swappedSeq_swapNeeded, sortedArray_merged_right_right_swappedSeq_m_1.io_z0, sortedArray_merged_right_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_157 = mux(sortedArray_merged_right_right_left_swappedSeq_swapNeeded, sortedArray_merged_right_right_swappedSeq_m.io_z0, sortedArray_merged_right_right_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_right_right_swappedSeq_swapNeeded_T = lt(sortedArray_merged_right_right_swappedSeq_m_1.io_z1, sortedArray_merged_right_right_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 43:94]
    node sortedArray_merged_right_right_right_swappedSeq_swapNeeded = eq(_sortedArray_merged_right_right_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_158 = mux(sortedArray_merged_right_right_right_swappedSeq_swapNeeded, sortedArray_merged_right_right_swappedSeq_m_1.io_z1, sortedArray_merged_right_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_159 = mux(sortedArray_merged_right_right_right_swappedSeq_swapNeeded, sortedArray_merged_right_right_swappedSeq_m.io_z1, sortedArray_merged_right_right_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _WIRE_0 = sortedArray_merged_left_left_left_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_1 = sortedArray_merged_left_left_left_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_2 = sortedArray_merged_left_left_right_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_3 = sortedArray_merged_left_left_right_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_4 = sortedArray_merged_left_right_left_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_5 = sortedArray_merged_left_right_left_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_6 = sortedArray_merged_left_right_right_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_7 = sortedArray_merged_left_right_right_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_8 = sortedArray_merged_right_left_left_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_9 = sortedArray_merged_right_left_left_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_10 = sortedArray_merged_right_left_right_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_11 = sortedArray_merged_right_left_right_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_12 = sortedArray_merged_right_right_left_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_13 = sortedArray_merged_right_right_left_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_14 = sortedArray_merged_right_right_right_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_15 = sortedArray_merged_right_right_right_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    io_z_0 <= _WIRE_0 @[BitonicSorterModule.scala 111:8]
    io_z_1 <= _WIRE_1 @[BitonicSorterModule.scala 111:8]
    io_z_2 <= _WIRE_2 @[BitonicSorterModule.scala 111:8]
    io_z_3 <= _WIRE_3 @[BitonicSorterModule.scala 111:8]
    io_z_4 <= _WIRE_4 @[BitonicSorterModule.scala 111:8]
    io_z_5 <= _WIRE_5 @[BitonicSorterModule.scala 111:8]
    io_z_6 <= _WIRE_6 @[BitonicSorterModule.scala 111:8]
    io_z_7 <= _WIRE_7 @[BitonicSorterModule.scala 111:8]
    io_z_8 <= _WIRE_8 @[BitonicSorterModule.scala 111:8]
    io_z_9 <= _WIRE_9 @[BitonicSorterModule.scala 111:8]
    io_z_10 <= _WIRE_10 @[BitonicSorterModule.scala 111:8]
    io_z_11 <= _WIRE_11 @[BitonicSorterModule.scala 111:8]
    io_z_12 <= _WIRE_12 @[BitonicSorterModule.scala 111:8]
    io_z_13 <= _WIRE_13 @[BitonicSorterModule.scala 111:8]
    io_z_14 <= _WIRE_14 @[BitonicSorterModule.scala 111:8]
    io_z_15 <= _WIRE_15 @[BitonicSorterModule.scala 111:8]
    sortedArray_left_left_left_merged_swappedSeq_m.clock <= clock
    sortedArray_left_left_left_merged_swappedSeq_m.reset <= reset
    sortedArray_left_left_left_merged_swappedSeq_m.io_a0 <= _GEN_0
    sortedArray_left_left_left_merged_swappedSeq_m.io_a1 <= _GEN_1
    sortedArray_left_left_right_merged_swappedSeq_m.clock <= clock
    sortedArray_left_left_right_merged_swappedSeq_m.reset <= reset
    sortedArray_left_left_right_merged_swappedSeq_m.io_a0 <= _GEN_2
    sortedArray_left_left_right_merged_swappedSeq_m.io_a1 <= _GEN_3
    sortedArray_left_left_merged_swappedSeq_m.clock <= clock
    sortedArray_left_left_merged_swappedSeq_m.reset <= reset
    sortedArray_left_left_merged_swappedSeq_m.io_a0 <= _GEN_4
    sortedArray_left_left_merged_swappedSeq_m.io_a1 <= _GEN_5
    sortedArray_left_left_merged_swappedSeq_m_1.clock <= clock
    sortedArray_left_left_merged_swappedSeq_m_1.reset <= reset
    sortedArray_left_left_merged_swappedSeq_m_1.io_a0 <= _GEN_6
    sortedArray_left_left_merged_swappedSeq_m_1.io_a1 <= _GEN_7
    sortedArray_left_left_merged_left_swappedSeq_m.clock <= clock
    sortedArray_left_left_merged_left_swappedSeq_m.reset <= reset
    sortedArray_left_left_merged_left_swappedSeq_m.io_a0 <= _GEN_8
    sortedArray_left_left_merged_left_swappedSeq_m.io_a1 <= _GEN_9
    sortedArray_left_left_merged_right_swappedSeq_m.clock <= clock
    sortedArray_left_left_merged_right_swappedSeq_m.reset <= reset
    sortedArray_left_left_merged_right_swappedSeq_m.io_a0 <= _GEN_10
    sortedArray_left_left_merged_right_swappedSeq_m.io_a1 <= _GEN_11
    sortedArray_left_right_left_merged_swappedSeq_m.clock <= clock
    sortedArray_left_right_left_merged_swappedSeq_m.reset <= reset
    sortedArray_left_right_left_merged_swappedSeq_m.io_a0 <= _GEN_12
    sortedArray_left_right_left_merged_swappedSeq_m.io_a1 <= _GEN_13
    sortedArray_left_right_right_merged_swappedSeq_m.clock <= clock
    sortedArray_left_right_right_merged_swappedSeq_m.reset <= reset
    sortedArray_left_right_right_merged_swappedSeq_m.io_a0 <= _GEN_14
    sortedArray_left_right_right_merged_swappedSeq_m.io_a1 <= _GEN_15
    sortedArray_left_right_merged_swappedSeq_m.clock <= clock
    sortedArray_left_right_merged_swappedSeq_m.reset <= reset
    sortedArray_left_right_merged_swappedSeq_m.io_a0 <= _GEN_16
    sortedArray_left_right_merged_swappedSeq_m.io_a1 <= _GEN_17
    sortedArray_left_right_merged_swappedSeq_m_1.clock <= clock
    sortedArray_left_right_merged_swappedSeq_m_1.reset <= reset
    sortedArray_left_right_merged_swappedSeq_m_1.io_a0 <= _GEN_18
    sortedArray_left_right_merged_swappedSeq_m_1.io_a1 <= _GEN_19
    sortedArray_left_right_merged_left_swappedSeq_m.clock <= clock
    sortedArray_left_right_merged_left_swappedSeq_m.reset <= reset
    sortedArray_left_right_merged_left_swappedSeq_m.io_a0 <= _GEN_20
    sortedArray_left_right_merged_left_swappedSeq_m.io_a1 <= _GEN_21
    sortedArray_left_right_merged_right_swappedSeq_m.clock <= clock
    sortedArray_left_right_merged_right_swappedSeq_m.reset <= reset
    sortedArray_left_right_merged_right_swappedSeq_m.io_a0 <= _GEN_22
    sortedArray_left_right_merged_right_swappedSeq_m.io_a1 <= _GEN_23
    sortedArray_left_merged_swappedSeq_m.clock <= clock
    sortedArray_left_merged_swappedSeq_m.reset <= reset
    sortedArray_left_merged_swappedSeq_m.io_a0 <= _GEN_24
    sortedArray_left_merged_swappedSeq_m.io_a1 <= _GEN_25
    sortedArray_left_merged_swappedSeq_m_1.clock <= clock
    sortedArray_left_merged_swappedSeq_m_1.reset <= reset
    sortedArray_left_merged_swappedSeq_m_1.io_a0 <= _GEN_26
    sortedArray_left_merged_swappedSeq_m_1.io_a1 <= _GEN_27
    sortedArray_left_merged_swappedSeq_m_2.clock <= clock
    sortedArray_left_merged_swappedSeq_m_2.reset <= reset
    sortedArray_left_merged_swappedSeq_m_2.io_a0 <= _GEN_28
    sortedArray_left_merged_swappedSeq_m_2.io_a1 <= _GEN_29
    sortedArray_left_merged_swappedSeq_m_3.clock <= clock
    sortedArray_left_merged_swappedSeq_m_3.reset <= reset
    sortedArray_left_merged_swappedSeq_m_3.io_a0 <= _GEN_30
    sortedArray_left_merged_swappedSeq_m_3.io_a1 <= _GEN_31
    sortedArray_left_merged_left_swappedSeq_m.clock <= clock
    sortedArray_left_merged_left_swappedSeq_m.reset <= reset
    sortedArray_left_merged_left_swappedSeq_m.io_a0 <= _GEN_32
    sortedArray_left_merged_left_swappedSeq_m.io_a1 <= _GEN_33
    sortedArray_left_merged_left_swappedSeq_m_1.clock <= clock
    sortedArray_left_merged_left_swappedSeq_m_1.reset <= reset
    sortedArray_left_merged_left_swappedSeq_m_1.io_a0 <= _GEN_34
    sortedArray_left_merged_left_swappedSeq_m_1.io_a1 <= _GEN_35
    sortedArray_left_merged_left_left_swappedSeq_m.clock <= clock
    sortedArray_left_merged_left_left_swappedSeq_m.reset <= reset
    sortedArray_left_merged_left_left_swappedSeq_m.io_a0 <= _GEN_36
    sortedArray_left_merged_left_left_swappedSeq_m.io_a1 <= _GEN_37
    sortedArray_left_merged_left_right_swappedSeq_m.clock <= clock
    sortedArray_left_merged_left_right_swappedSeq_m.reset <= reset
    sortedArray_left_merged_left_right_swappedSeq_m.io_a0 <= _GEN_38
    sortedArray_left_merged_left_right_swappedSeq_m.io_a1 <= _GEN_39
    sortedArray_left_merged_right_swappedSeq_m.clock <= clock
    sortedArray_left_merged_right_swappedSeq_m.reset <= reset
    sortedArray_left_merged_right_swappedSeq_m.io_a0 <= _GEN_40
    sortedArray_left_merged_right_swappedSeq_m.io_a1 <= _GEN_41
    sortedArray_left_merged_right_swappedSeq_m_1.clock <= clock
    sortedArray_left_merged_right_swappedSeq_m_1.reset <= reset
    sortedArray_left_merged_right_swappedSeq_m_1.io_a0 <= _GEN_42
    sortedArray_left_merged_right_swappedSeq_m_1.io_a1 <= _GEN_43
    sortedArray_left_merged_right_left_swappedSeq_m.clock <= clock
    sortedArray_left_merged_right_left_swappedSeq_m.reset <= reset
    sortedArray_left_merged_right_left_swappedSeq_m.io_a0 <= _GEN_44
    sortedArray_left_merged_right_left_swappedSeq_m.io_a1 <= _GEN_45
    sortedArray_left_merged_right_right_swappedSeq_m.clock <= clock
    sortedArray_left_merged_right_right_swappedSeq_m.reset <= reset
    sortedArray_left_merged_right_right_swappedSeq_m.io_a0 <= _GEN_46
    sortedArray_left_merged_right_right_swappedSeq_m.io_a1 <= _GEN_47
    sortedArray_right_left_left_merged_swappedSeq_m.clock <= clock
    sortedArray_right_left_left_merged_swappedSeq_m.reset <= reset
    sortedArray_right_left_left_merged_swappedSeq_m.io_a0 <= _GEN_48
    sortedArray_right_left_left_merged_swappedSeq_m.io_a1 <= _GEN_49
    sortedArray_right_left_right_merged_swappedSeq_m.clock <= clock
    sortedArray_right_left_right_merged_swappedSeq_m.reset <= reset
    sortedArray_right_left_right_merged_swappedSeq_m.io_a0 <= _GEN_50
    sortedArray_right_left_right_merged_swappedSeq_m.io_a1 <= _GEN_51
    sortedArray_right_left_merged_swappedSeq_m.clock <= clock
    sortedArray_right_left_merged_swappedSeq_m.reset <= reset
    sortedArray_right_left_merged_swappedSeq_m.io_a0 <= _GEN_52
    sortedArray_right_left_merged_swappedSeq_m.io_a1 <= _GEN_53
    sortedArray_right_left_merged_swappedSeq_m_1.clock <= clock
    sortedArray_right_left_merged_swappedSeq_m_1.reset <= reset
    sortedArray_right_left_merged_swappedSeq_m_1.io_a0 <= _GEN_54
    sortedArray_right_left_merged_swappedSeq_m_1.io_a1 <= _GEN_55
    sortedArray_right_left_merged_left_swappedSeq_m.clock <= clock
    sortedArray_right_left_merged_left_swappedSeq_m.reset <= reset
    sortedArray_right_left_merged_left_swappedSeq_m.io_a0 <= _GEN_56
    sortedArray_right_left_merged_left_swappedSeq_m.io_a1 <= _GEN_57
    sortedArray_right_left_merged_right_swappedSeq_m.clock <= clock
    sortedArray_right_left_merged_right_swappedSeq_m.reset <= reset
    sortedArray_right_left_merged_right_swappedSeq_m.io_a0 <= _GEN_58
    sortedArray_right_left_merged_right_swappedSeq_m.io_a1 <= _GEN_59
    sortedArray_right_right_left_merged_swappedSeq_m.clock <= clock
    sortedArray_right_right_left_merged_swappedSeq_m.reset <= reset
    sortedArray_right_right_left_merged_swappedSeq_m.io_a0 <= _GEN_60
    sortedArray_right_right_left_merged_swappedSeq_m.io_a1 <= _GEN_61
    sortedArray_right_right_right_merged_swappedSeq_m.clock <= clock
    sortedArray_right_right_right_merged_swappedSeq_m.reset <= reset
    sortedArray_right_right_right_merged_swappedSeq_m.io_a0 <= _GEN_62
    sortedArray_right_right_right_merged_swappedSeq_m.io_a1 <= _GEN_63
    sortedArray_right_right_merged_swappedSeq_m.clock <= clock
    sortedArray_right_right_merged_swappedSeq_m.reset <= reset
    sortedArray_right_right_merged_swappedSeq_m.io_a0 <= _GEN_64
    sortedArray_right_right_merged_swappedSeq_m.io_a1 <= _GEN_65
    sortedArray_right_right_merged_swappedSeq_m_1.clock <= clock
    sortedArray_right_right_merged_swappedSeq_m_1.reset <= reset
    sortedArray_right_right_merged_swappedSeq_m_1.io_a0 <= _GEN_66
    sortedArray_right_right_merged_swappedSeq_m_1.io_a1 <= _GEN_67
    sortedArray_right_right_merged_left_swappedSeq_m.clock <= clock
    sortedArray_right_right_merged_left_swappedSeq_m.reset <= reset
    sortedArray_right_right_merged_left_swappedSeq_m.io_a0 <= _GEN_68
    sortedArray_right_right_merged_left_swappedSeq_m.io_a1 <= _GEN_69
    sortedArray_right_right_merged_right_swappedSeq_m.clock <= clock
    sortedArray_right_right_merged_right_swappedSeq_m.reset <= reset
    sortedArray_right_right_merged_right_swappedSeq_m.io_a0 <= _GEN_70
    sortedArray_right_right_merged_right_swappedSeq_m.io_a1 <= _GEN_71
    sortedArray_right_merged_swappedSeq_m.clock <= clock
    sortedArray_right_merged_swappedSeq_m.reset <= reset
    sortedArray_right_merged_swappedSeq_m.io_a0 <= _GEN_72
    sortedArray_right_merged_swappedSeq_m.io_a1 <= _GEN_73
    sortedArray_right_merged_swappedSeq_m_1.clock <= clock
    sortedArray_right_merged_swappedSeq_m_1.reset <= reset
    sortedArray_right_merged_swappedSeq_m_1.io_a0 <= _GEN_74
    sortedArray_right_merged_swappedSeq_m_1.io_a1 <= _GEN_75
    sortedArray_right_merged_swappedSeq_m_2.clock <= clock
    sortedArray_right_merged_swappedSeq_m_2.reset <= reset
    sortedArray_right_merged_swappedSeq_m_2.io_a0 <= _GEN_76
    sortedArray_right_merged_swappedSeq_m_2.io_a1 <= _GEN_77
    sortedArray_right_merged_swappedSeq_m_3.clock <= clock
    sortedArray_right_merged_swappedSeq_m_3.reset <= reset
    sortedArray_right_merged_swappedSeq_m_3.io_a0 <= _GEN_78
    sortedArray_right_merged_swappedSeq_m_3.io_a1 <= _GEN_79
    sortedArray_right_merged_left_swappedSeq_m.clock <= clock
    sortedArray_right_merged_left_swappedSeq_m.reset <= reset
    sortedArray_right_merged_left_swappedSeq_m.io_a0 <= _GEN_80
    sortedArray_right_merged_left_swappedSeq_m.io_a1 <= _GEN_81
    sortedArray_right_merged_left_swappedSeq_m_1.clock <= clock
    sortedArray_right_merged_left_swappedSeq_m_1.reset <= reset
    sortedArray_right_merged_left_swappedSeq_m_1.io_a0 <= _GEN_82
    sortedArray_right_merged_left_swappedSeq_m_1.io_a1 <= _GEN_83
    sortedArray_right_merged_left_left_swappedSeq_m.clock <= clock
    sortedArray_right_merged_left_left_swappedSeq_m.reset <= reset
    sortedArray_right_merged_left_left_swappedSeq_m.io_a0 <= _GEN_84
    sortedArray_right_merged_left_left_swappedSeq_m.io_a1 <= _GEN_85
    sortedArray_right_merged_left_right_swappedSeq_m.clock <= clock
    sortedArray_right_merged_left_right_swappedSeq_m.reset <= reset
    sortedArray_right_merged_left_right_swappedSeq_m.io_a0 <= _GEN_86
    sortedArray_right_merged_left_right_swappedSeq_m.io_a1 <= _GEN_87
    sortedArray_right_merged_right_swappedSeq_m.clock <= clock
    sortedArray_right_merged_right_swappedSeq_m.reset <= reset
    sortedArray_right_merged_right_swappedSeq_m.io_a0 <= _GEN_88
    sortedArray_right_merged_right_swappedSeq_m.io_a1 <= _GEN_89
    sortedArray_right_merged_right_swappedSeq_m_1.clock <= clock
    sortedArray_right_merged_right_swappedSeq_m_1.reset <= reset
    sortedArray_right_merged_right_swappedSeq_m_1.io_a0 <= _GEN_90
    sortedArray_right_merged_right_swappedSeq_m_1.io_a1 <= _GEN_91
    sortedArray_right_merged_right_left_swappedSeq_m.clock <= clock
    sortedArray_right_merged_right_left_swappedSeq_m.reset <= reset
    sortedArray_right_merged_right_left_swappedSeq_m.io_a0 <= _GEN_92
    sortedArray_right_merged_right_left_swappedSeq_m.io_a1 <= _GEN_93
    sortedArray_right_merged_right_right_swappedSeq_m.clock <= clock
    sortedArray_right_merged_right_right_swappedSeq_m.reset <= reset
    sortedArray_right_merged_right_right_swappedSeq_m.io_a0 <= _GEN_94
    sortedArray_right_merged_right_right_swappedSeq_m.io_a1 <= _GEN_95
    sortedArray_merged_swappedSeq_m.clock <= clock
    sortedArray_merged_swappedSeq_m.reset <= reset
    sortedArray_merged_swappedSeq_m.io_a0 <= _GEN_96
    sortedArray_merged_swappedSeq_m.io_a1 <= _GEN_97
    sortedArray_merged_swappedSeq_m_1.clock <= clock
    sortedArray_merged_swappedSeq_m_1.reset <= reset
    sortedArray_merged_swappedSeq_m_1.io_a0 <= _GEN_98
    sortedArray_merged_swappedSeq_m_1.io_a1 <= _GEN_99
    sortedArray_merged_swappedSeq_m_2.clock <= clock
    sortedArray_merged_swappedSeq_m_2.reset <= reset
    sortedArray_merged_swappedSeq_m_2.io_a0 <= _GEN_100
    sortedArray_merged_swappedSeq_m_2.io_a1 <= _GEN_101
    sortedArray_merged_swappedSeq_m_3.clock <= clock
    sortedArray_merged_swappedSeq_m_3.reset <= reset
    sortedArray_merged_swappedSeq_m_3.io_a0 <= _GEN_102
    sortedArray_merged_swappedSeq_m_3.io_a1 <= _GEN_103
    sortedArray_merged_swappedSeq_m_4.clock <= clock
    sortedArray_merged_swappedSeq_m_4.reset <= reset
    sortedArray_merged_swappedSeq_m_4.io_a0 <= _GEN_104
    sortedArray_merged_swappedSeq_m_4.io_a1 <= _GEN_105
    sortedArray_merged_swappedSeq_m_5.clock <= clock
    sortedArray_merged_swappedSeq_m_5.reset <= reset
    sortedArray_merged_swappedSeq_m_5.io_a0 <= _GEN_106
    sortedArray_merged_swappedSeq_m_5.io_a1 <= _GEN_107
    sortedArray_merged_swappedSeq_m_6.clock <= clock
    sortedArray_merged_swappedSeq_m_6.reset <= reset
    sortedArray_merged_swappedSeq_m_6.io_a0 <= _GEN_108
    sortedArray_merged_swappedSeq_m_6.io_a1 <= _GEN_109
    sortedArray_merged_swappedSeq_m_7.clock <= clock
    sortedArray_merged_swappedSeq_m_7.reset <= reset
    sortedArray_merged_swappedSeq_m_7.io_a0 <= _GEN_110
    sortedArray_merged_swappedSeq_m_7.io_a1 <= _GEN_111
    sortedArray_merged_left_swappedSeq_m.clock <= clock
    sortedArray_merged_left_swappedSeq_m.reset <= reset
    sortedArray_merged_left_swappedSeq_m.io_a0 <= _GEN_112
    sortedArray_merged_left_swappedSeq_m.io_a1 <= _GEN_113
    sortedArray_merged_left_swappedSeq_m_1.clock <= clock
    sortedArray_merged_left_swappedSeq_m_1.reset <= reset
    sortedArray_merged_left_swappedSeq_m_1.io_a0 <= _GEN_114
    sortedArray_merged_left_swappedSeq_m_1.io_a1 <= _GEN_115
    sortedArray_merged_left_swappedSeq_m_2.clock <= clock
    sortedArray_merged_left_swappedSeq_m_2.reset <= reset
    sortedArray_merged_left_swappedSeq_m_2.io_a0 <= _GEN_116
    sortedArray_merged_left_swappedSeq_m_2.io_a1 <= _GEN_117
    sortedArray_merged_left_swappedSeq_m_3.clock <= clock
    sortedArray_merged_left_swappedSeq_m_3.reset <= reset
    sortedArray_merged_left_swappedSeq_m_3.io_a0 <= _GEN_118
    sortedArray_merged_left_swappedSeq_m_3.io_a1 <= _GEN_119
    sortedArray_merged_left_left_swappedSeq_m.clock <= clock
    sortedArray_merged_left_left_swappedSeq_m.reset <= reset
    sortedArray_merged_left_left_swappedSeq_m.io_a0 <= _GEN_120
    sortedArray_merged_left_left_swappedSeq_m.io_a1 <= _GEN_121
    sortedArray_merged_left_left_swappedSeq_m_1.clock <= clock
    sortedArray_merged_left_left_swappedSeq_m_1.reset <= reset
    sortedArray_merged_left_left_swappedSeq_m_1.io_a0 <= _GEN_122
    sortedArray_merged_left_left_swappedSeq_m_1.io_a1 <= _GEN_123
    sortedArray_merged_left_left_left_swappedSeq_m.clock <= clock
    sortedArray_merged_left_left_left_swappedSeq_m.reset <= reset
    sortedArray_merged_left_left_left_swappedSeq_m.io_a0 <= _GEN_124
    sortedArray_merged_left_left_left_swappedSeq_m.io_a1 <= _GEN_125
    sortedArray_merged_left_left_right_swappedSeq_m.clock <= clock
    sortedArray_merged_left_left_right_swappedSeq_m.reset <= reset
    sortedArray_merged_left_left_right_swappedSeq_m.io_a0 <= _GEN_126
    sortedArray_merged_left_left_right_swappedSeq_m.io_a1 <= _GEN_127
    sortedArray_merged_left_right_swappedSeq_m.clock <= clock
    sortedArray_merged_left_right_swappedSeq_m.reset <= reset
    sortedArray_merged_left_right_swappedSeq_m.io_a0 <= _GEN_128
    sortedArray_merged_left_right_swappedSeq_m.io_a1 <= _GEN_129
    sortedArray_merged_left_right_swappedSeq_m_1.clock <= clock
    sortedArray_merged_left_right_swappedSeq_m_1.reset <= reset
    sortedArray_merged_left_right_swappedSeq_m_1.io_a0 <= _GEN_130
    sortedArray_merged_left_right_swappedSeq_m_1.io_a1 <= _GEN_131
    sortedArray_merged_left_right_left_swappedSeq_m.clock <= clock
    sortedArray_merged_left_right_left_swappedSeq_m.reset <= reset
    sortedArray_merged_left_right_left_swappedSeq_m.io_a0 <= _GEN_132
    sortedArray_merged_left_right_left_swappedSeq_m.io_a1 <= _GEN_133
    sortedArray_merged_left_right_right_swappedSeq_m.clock <= clock
    sortedArray_merged_left_right_right_swappedSeq_m.reset <= reset
    sortedArray_merged_left_right_right_swappedSeq_m.io_a0 <= _GEN_134
    sortedArray_merged_left_right_right_swappedSeq_m.io_a1 <= _GEN_135
    sortedArray_merged_right_swappedSeq_m.clock <= clock
    sortedArray_merged_right_swappedSeq_m.reset <= reset
    sortedArray_merged_right_swappedSeq_m.io_a0 <= _GEN_136
    sortedArray_merged_right_swappedSeq_m.io_a1 <= _GEN_137
    sortedArray_merged_right_swappedSeq_m_1.clock <= clock
    sortedArray_merged_right_swappedSeq_m_1.reset <= reset
    sortedArray_merged_right_swappedSeq_m_1.io_a0 <= _GEN_138
    sortedArray_merged_right_swappedSeq_m_1.io_a1 <= _GEN_139
    sortedArray_merged_right_swappedSeq_m_2.clock <= clock
    sortedArray_merged_right_swappedSeq_m_2.reset <= reset
    sortedArray_merged_right_swappedSeq_m_2.io_a0 <= _GEN_140
    sortedArray_merged_right_swappedSeq_m_2.io_a1 <= _GEN_141
    sortedArray_merged_right_swappedSeq_m_3.clock <= clock
    sortedArray_merged_right_swappedSeq_m_3.reset <= reset
    sortedArray_merged_right_swappedSeq_m_3.io_a0 <= _GEN_142
    sortedArray_merged_right_swappedSeq_m_3.io_a1 <= _GEN_143
    sortedArray_merged_right_left_swappedSeq_m.clock <= clock
    sortedArray_merged_right_left_swappedSeq_m.reset <= reset
    sortedArray_merged_right_left_swappedSeq_m.io_a0 <= _GEN_144
    sortedArray_merged_right_left_swappedSeq_m.io_a1 <= _GEN_145
    sortedArray_merged_right_left_swappedSeq_m_1.clock <= clock
    sortedArray_merged_right_left_swappedSeq_m_1.reset <= reset
    sortedArray_merged_right_left_swappedSeq_m_1.io_a0 <= _GEN_146
    sortedArray_merged_right_left_swappedSeq_m_1.io_a1 <= _GEN_147
    sortedArray_merged_right_left_left_swappedSeq_m.clock <= clock
    sortedArray_merged_right_left_left_swappedSeq_m.reset <= reset
    sortedArray_merged_right_left_left_swappedSeq_m.io_a0 <= _GEN_148
    sortedArray_merged_right_left_left_swappedSeq_m.io_a1 <= _GEN_149
    sortedArray_merged_right_left_right_swappedSeq_m.clock <= clock
    sortedArray_merged_right_left_right_swappedSeq_m.reset <= reset
    sortedArray_merged_right_left_right_swappedSeq_m.io_a0 <= _GEN_150
    sortedArray_merged_right_left_right_swappedSeq_m.io_a1 <= _GEN_151
    sortedArray_merged_right_right_swappedSeq_m.clock <= clock
    sortedArray_merged_right_right_swappedSeq_m.reset <= reset
    sortedArray_merged_right_right_swappedSeq_m.io_a0 <= _GEN_152
    sortedArray_merged_right_right_swappedSeq_m.io_a1 <= _GEN_153
    sortedArray_merged_right_right_swappedSeq_m_1.clock <= clock
    sortedArray_merged_right_right_swappedSeq_m_1.reset <= reset
    sortedArray_merged_right_right_swappedSeq_m_1.io_a0 <= _GEN_154
    sortedArray_merged_right_right_swappedSeq_m_1.io_a1 <= _GEN_155
    sortedArray_merged_right_right_left_swappedSeq_m.clock <= clock
    sortedArray_merged_right_right_left_swappedSeq_m.reset <= reset
    sortedArray_merged_right_right_left_swappedSeq_m.io_a0 <= _GEN_156
    sortedArray_merged_right_right_left_swappedSeq_m.io_a1 <= _GEN_157
    sortedArray_merged_right_right_right_swappedSeq_m.clock <= clock
    sortedArray_merged_right_right_right_swappedSeq_m.reset <= reset
    sortedArray_merged_right_right_right_swappedSeq_m.io_a0 <= _GEN_158
    sortedArray_merged_right_right_right_swappedSeq_m.io_a1 <= _GEN_159

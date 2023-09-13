FIRRTL version 1.1.0
circuit BitonicSorterModule :
  module Passthrough :
    input clock : Clock
    input reset : UInt<1>
    input io_a0 : UInt<8>
    input io_a1 : UInt<8>
    output io_z0 : UInt<8>
    output io_z1 : UInt<8>

    io_z0 <= io_a0 @[BitonicSorterModule.scala 20:9]
    io_z1 <= io_a1 @[BitonicSorterModule.scala 21:9]

  module BitonicSorterModule :
    input clock : Clock
    input reset : UInt<1>
    input io_a_0 : UInt<8>
    input io_a_1 : UInt<8>
    input io_a_2 : UInt<8>
    input io_a_3 : UInt<8>
    input io_a_4 : UInt<8>
    input io_a_5 : UInt<8>
    input io_a_6 : UInt<8>
    input io_a_7 : UInt<8>
    output io_z_0 : UInt<8>
    output io_z_1 : UInt<8>
    output io_z_2 : UInt<8>
    output io_z_3 : UInt<8>
    output io_z_4 : UInt<8>
    output io_z_5 : UInt<8>
    output io_z_6 : UInt<8>
    output io_z_7 : UInt<8>

    inst sortedArray_left_left_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_right_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_left_merged_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_left_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_right_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_right_merged_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m_2 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_swappedSeq_m_3 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_left_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_swappedSeq_m_1 of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_left_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    inst sortedArray_merged_right_right_swappedSeq_m of Passthrough @[BitonicSorterModule.scala 37:21]
    node _sortedArray_left_left_merged_swappedSeq_swapNeeded_T = lt(io_a_1, io_a_0) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_left_left_merged_swappedSeq_swapNeeded = eq(_sortedArray_left_left_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_0 = mux(sortedArray_left_left_merged_swappedSeq_swapNeeded, io_a_1, io_a_0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_1 = mux(sortedArray_left_left_merged_swappedSeq_swapNeeded, io_a_0, io_a_1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_right_merged_swappedSeq_swapNeeded_T = lt(io_a_3, io_a_2) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_left_right_merged_swappedSeq_swapNeeded = eq(_sortedArray_left_right_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_2 = mux(sortedArray_left_right_merged_swappedSeq_swapNeeded, io_a_3, io_a_2) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_3 = mux(sortedArray_left_right_merged_swappedSeq_swapNeeded, io_a_2, io_a_3) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_swappedSeq_swapNeeded_T = lt(sortedArray_left_right_merged_swappedSeq_m.io_z0, sortedArray_left_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_left_merged_swappedSeq_swapNeeded = eq(_sortedArray_left_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_4 = mux(sortedArray_left_merged_swappedSeq_swapNeeded, sortedArray_left_right_merged_swappedSeq_m.io_z0, sortedArray_left_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_5 = mux(sortedArray_left_merged_swappedSeq_swapNeeded, sortedArray_left_left_merged_swappedSeq_m.io_z0, sortedArray_left_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_swappedSeq_swapNeeded_T_1 = lt(sortedArray_left_right_merged_swappedSeq_m.io_z1, sortedArray_left_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_left_merged_swappedSeq_swapNeeded_1 = eq(_sortedArray_left_merged_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_6 = mux(sortedArray_left_merged_swappedSeq_swapNeeded_1, sortedArray_left_right_merged_swappedSeq_m.io_z1, sortedArray_left_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_7 = mux(sortedArray_left_merged_swappedSeq_swapNeeded_1, sortedArray_left_left_merged_swappedSeq_m.io_z1, sortedArray_left_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_left_swappedSeq_swapNeeded_T = lt(sortedArray_left_merged_swappedSeq_m_1.io_z0, sortedArray_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_left_merged_left_swappedSeq_swapNeeded = eq(_sortedArray_left_merged_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_8 = mux(sortedArray_left_merged_left_swappedSeq_swapNeeded, sortedArray_left_merged_swappedSeq_m_1.io_z0, sortedArray_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_9 = mux(sortedArray_left_merged_left_swappedSeq_swapNeeded, sortedArray_left_merged_swappedSeq_m.io_z0, sortedArray_left_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_left_merged_right_swappedSeq_swapNeeded_T = lt(sortedArray_left_merged_swappedSeq_m_1.io_z1, sortedArray_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_left_merged_right_swappedSeq_swapNeeded = eq(_sortedArray_left_merged_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_10 = mux(sortedArray_left_merged_right_swappedSeq_swapNeeded, sortedArray_left_merged_swappedSeq_m_1.io_z1, sortedArray_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_11 = mux(sortedArray_left_merged_right_swappedSeq_swapNeeded, sortedArray_left_merged_swappedSeq_m.io_z1, sortedArray_left_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_left_merged_swappedSeq_swapNeeded_T = lt(io_a_5, io_a_4) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_right_left_merged_swappedSeq_swapNeeded = eq(_sortedArray_right_left_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_12 = mux(sortedArray_right_left_merged_swappedSeq_swapNeeded, io_a_5, io_a_4) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_13 = mux(sortedArray_right_left_merged_swappedSeq_swapNeeded, io_a_4, io_a_5) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_right_merged_swappedSeq_swapNeeded_T = lt(io_a_7, io_a_6) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_right_right_merged_swappedSeq_swapNeeded = eq(_sortedArray_right_right_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_14 = mux(sortedArray_right_right_merged_swappedSeq_swapNeeded, io_a_7, io_a_6) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_15 = mux(sortedArray_right_right_merged_swappedSeq_swapNeeded, io_a_6, io_a_7) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_swappedSeq_swapNeeded_T = lt(sortedArray_right_right_merged_swappedSeq_m.io_z0, sortedArray_right_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_right_merged_swappedSeq_swapNeeded = eq(_sortedArray_right_merged_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_16 = mux(sortedArray_right_merged_swappedSeq_swapNeeded, sortedArray_right_right_merged_swappedSeq_m.io_z0, sortedArray_right_left_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_17 = mux(sortedArray_right_merged_swappedSeq_swapNeeded, sortedArray_right_left_merged_swappedSeq_m.io_z0, sortedArray_right_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_swappedSeq_swapNeeded_T_1 = lt(sortedArray_right_right_merged_swappedSeq_m.io_z1, sortedArray_right_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_right_merged_swappedSeq_swapNeeded_1 = eq(_sortedArray_right_merged_swappedSeq_swapNeeded_T_1, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_18 = mux(sortedArray_right_merged_swappedSeq_swapNeeded_1, sortedArray_right_right_merged_swappedSeq_m.io_z1, sortedArray_right_left_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_19 = mux(sortedArray_right_merged_swappedSeq_swapNeeded_1, sortedArray_right_left_merged_swappedSeq_m.io_z1, sortedArray_right_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_left_swappedSeq_swapNeeded_T = lt(sortedArray_right_merged_swappedSeq_m_1.io_z0, sortedArray_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_right_merged_left_swappedSeq_swapNeeded = eq(_sortedArray_right_merged_left_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_20 = mux(sortedArray_right_merged_left_swappedSeq_swapNeeded, sortedArray_right_merged_swappedSeq_m_1.io_z0, sortedArray_right_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_21 = mux(sortedArray_right_merged_left_swappedSeq_swapNeeded, sortedArray_right_merged_swappedSeq_m.io_z0, sortedArray_right_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_right_merged_right_swappedSeq_swapNeeded_T = lt(sortedArray_right_merged_swappedSeq_m_1.io_z1, sortedArray_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_right_merged_right_swappedSeq_swapNeeded = eq(_sortedArray_right_merged_right_swappedSeq_swapNeeded_T, UInt<1>("h0")) @[BitonicSorterModule.scala 38:35]
    node _GEN_22 = mux(sortedArray_right_merged_right_swappedSeq_swapNeeded, sortedArray_right_merged_swappedSeq_m_1.io_z1, sortedArray_right_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_23 = mux(sortedArray_right_merged_right_swappedSeq_swapNeeded, sortedArray_right_merged_swappedSeq_m.io_z1, sortedArray_right_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T = lt(sortedArray_right_merged_left_swappedSeq_m.io_z0, sortedArray_left_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_swappedSeq_swapNeeded = eq(_sortedArray_merged_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_24 = mux(sortedArray_merged_swappedSeq_swapNeeded, sortedArray_right_merged_left_swappedSeq_m.io_z0, sortedArray_left_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_25 = mux(sortedArray_merged_swappedSeq_swapNeeded, sortedArray_left_merged_left_swappedSeq_m.io_z0, sortedArray_right_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T_1 = lt(sortedArray_right_merged_left_swappedSeq_m.io_z1, sortedArray_left_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_swappedSeq_swapNeeded_1 = eq(_sortedArray_merged_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_26 = mux(sortedArray_merged_swappedSeq_swapNeeded_1, sortedArray_right_merged_left_swappedSeq_m.io_z1, sortedArray_left_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_27 = mux(sortedArray_merged_swappedSeq_swapNeeded_1, sortedArray_left_merged_left_swappedSeq_m.io_z1, sortedArray_right_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T_2 = lt(sortedArray_right_merged_right_swappedSeq_m.io_z0, sortedArray_left_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_swappedSeq_swapNeeded_2 = eq(_sortedArray_merged_swappedSeq_swapNeeded_T_2, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_28 = mux(sortedArray_merged_swappedSeq_swapNeeded_2, sortedArray_right_merged_right_swappedSeq_m.io_z0, sortedArray_left_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_29 = mux(sortedArray_merged_swappedSeq_swapNeeded_2, sortedArray_left_merged_right_swappedSeq_m.io_z0, sortedArray_right_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_swappedSeq_swapNeeded_T_3 = lt(sortedArray_right_merged_right_swappedSeq_m.io_z1, sortedArray_left_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_swappedSeq_swapNeeded_3 = eq(_sortedArray_merged_swappedSeq_swapNeeded_T_3, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_30 = mux(sortedArray_merged_swappedSeq_swapNeeded_3, sortedArray_right_merged_right_swappedSeq_m.io_z1, sortedArray_left_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_31 = mux(sortedArray_merged_swappedSeq_swapNeeded_3, sortedArray_left_merged_right_swappedSeq_m.io_z1, sortedArray_right_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_swappedSeq_swapNeeded_T = lt(sortedArray_merged_swappedSeq_m_2.io_z0, sortedArray_merged_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_left_swappedSeq_swapNeeded = eq(_sortedArray_merged_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_32 = mux(sortedArray_merged_left_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m_2.io_z0, sortedArray_merged_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_33 = mux(sortedArray_merged_left_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m.io_z0, sortedArray_merged_swappedSeq_m_2.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_swappedSeq_swapNeeded_T_1 = lt(sortedArray_merged_swappedSeq_m_3.io_z0, sortedArray_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_left_swappedSeq_swapNeeded_1 = eq(_sortedArray_merged_left_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_34 = mux(sortedArray_merged_left_swappedSeq_swapNeeded_1, sortedArray_merged_swappedSeq_m_3.io_z0, sortedArray_merged_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_35 = mux(sortedArray_merged_left_swappedSeq_swapNeeded_1, sortedArray_merged_swappedSeq_m_1.io_z0, sortedArray_merged_swappedSeq_m_3.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_left_swappedSeq_swapNeeded_T = lt(sortedArray_merged_left_swappedSeq_m_1.io_z0, sortedArray_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_left_left_swappedSeq_swapNeeded = eq(_sortedArray_merged_left_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_36 = mux(sortedArray_merged_left_left_swappedSeq_swapNeeded, sortedArray_merged_left_swappedSeq_m_1.io_z0, sortedArray_merged_left_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_37 = mux(sortedArray_merged_left_left_swappedSeq_swapNeeded, sortedArray_merged_left_swappedSeq_m.io_z0, sortedArray_merged_left_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_left_right_swappedSeq_swapNeeded_T = lt(sortedArray_merged_left_swappedSeq_m_1.io_z1, sortedArray_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_left_right_swappedSeq_swapNeeded = eq(_sortedArray_merged_left_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_38 = mux(sortedArray_merged_left_right_swappedSeq_swapNeeded, sortedArray_merged_left_swappedSeq_m_1.io_z1, sortedArray_merged_left_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_39 = mux(sortedArray_merged_left_right_swappedSeq_swapNeeded, sortedArray_merged_left_swappedSeq_m.io_z1, sortedArray_merged_left_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_swappedSeq_swapNeeded_T = lt(sortedArray_merged_swappedSeq_m_2.io_z1, sortedArray_merged_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_right_swappedSeq_swapNeeded = eq(_sortedArray_merged_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_40 = mux(sortedArray_merged_right_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m_2.io_z1, sortedArray_merged_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_41 = mux(sortedArray_merged_right_swappedSeq_swapNeeded, sortedArray_merged_swappedSeq_m.io_z1, sortedArray_merged_swappedSeq_m_2.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_swappedSeq_swapNeeded_T_1 = lt(sortedArray_merged_swappedSeq_m_3.io_z1, sortedArray_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_right_swappedSeq_swapNeeded_1 = eq(_sortedArray_merged_right_swappedSeq_swapNeeded_T_1, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_42 = mux(sortedArray_merged_right_swappedSeq_swapNeeded_1, sortedArray_merged_swappedSeq_m_3.io_z1, sortedArray_merged_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_43 = mux(sortedArray_merged_right_swappedSeq_swapNeeded_1, sortedArray_merged_swappedSeq_m_1.io_z1, sortedArray_merged_swappedSeq_m_3.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_left_swappedSeq_swapNeeded_T = lt(sortedArray_merged_right_swappedSeq_m_1.io_z0, sortedArray_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_right_left_swappedSeq_swapNeeded = eq(_sortedArray_merged_right_left_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_44 = mux(sortedArray_merged_right_left_swappedSeq_swapNeeded, sortedArray_merged_right_swappedSeq_m_1.io_z0, sortedArray_merged_right_swappedSeq_m.io_z0) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_45 = mux(sortedArray_merged_right_left_swappedSeq_swapNeeded, sortedArray_merged_right_swappedSeq_m.io_z0, sortedArray_merged_right_swappedSeq_m_1.io_z0) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _sortedArray_merged_right_right_swappedSeq_swapNeeded_T = lt(sortedArray_merged_right_swappedSeq_m_1.io_z1, sortedArray_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModuleTest.scala 7:80]
    node sortedArray_merged_right_right_swappedSeq_swapNeeded = eq(_sortedArray_merged_right_right_swappedSeq_swapNeeded_T, UInt<1>("h1")) @[BitonicSorterModule.scala 38:35]
    node _GEN_46 = mux(sortedArray_merged_right_right_swappedSeq_swapNeeded, sortedArray_merged_right_swappedSeq_m_1.io_z1, sortedArray_merged_right_swappedSeq_m.io_z1) @[BitonicSorterModule.scala 39:24 40:17 43:17]
    node _GEN_47 = mux(sortedArray_merged_right_right_swappedSeq_swapNeeded, sortedArray_merged_right_swappedSeq_m.io_z1, sortedArray_merged_right_swappedSeq_m_1.io_z1) @[BitonicSorterModule.scala 39:24 41:17 44:17]
    node _WIRE_0 = sortedArray_merged_left_left_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_1 = sortedArray_merged_left_left_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_2 = sortedArray_merged_left_right_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_3 = sortedArray_merged_left_right_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_4 = sortedArray_merged_right_left_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_5 = sortedArray_merged_right_left_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_6 = sortedArray_merged_right_right_swappedSeq_m.io_z0 @[BitonicSorterModule.scala 111:{18,18}]
    node _WIRE_7 = sortedArray_merged_right_right_swappedSeq_m.io_z1 @[BitonicSorterModule.scala 111:{18,18}]
    io_z_0 <= _WIRE_0 @[BitonicSorterModule.scala 111:8]
    io_z_1 <= _WIRE_1 @[BitonicSorterModule.scala 111:8]
    io_z_2 <= _WIRE_2 @[BitonicSorterModule.scala 111:8]
    io_z_3 <= _WIRE_3 @[BitonicSorterModule.scala 111:8]
    io_z_4 <= _WIRE_4 @[BitonicSorterModule.scala 111:8]
    io_z_5 <= _WIRE_5 @[BitonicSorterModule.scala 111:8]
    io_z_6 <= _WIRE_6 @[BitonicSorterModule.scala 111:8]
    io_z_7 <= _WIRE_7 @[BitonicSorterModule.scala 111:8]
    sortedArray_left_left_merged_swappedSeq_m.clock <= clock
    sortedArray_left_left_merged_swappedSeq_m.reset <= reset
    sortedArray_left_left_merged_swappedSeq_m.io_a0 <= _GEN_0
    sortedArray_left_left_merged_swappedSeq_m.io_a1 <= _GEN_1
    sortedArray_left_right_merged_swappedSeq_m.clock <= clock
    sortedArray_left_right_merged_swappedSeq_m.reset <= reset
    sortedArray_left_right_merged_swappedSeq_m.io_a0 <= _GEN_2
    sortedArray_left_right_merged_swappedSeq_m.io_a1 <= _GEN_3
    sortedArray_left_merged_swappedSeq_m.clock <= clock
    sortedArray_left_merged_swappedSeq_m.reset <= reset
    sortedArray_left_merged_swappedSeq_m.io_a0 <= _GEN_4
    sortedArray_left_merged_swappedSeq_m.io_a1 <= _GEN_5
    sortedArray_left_merged_swappedSeq_m_1.clock <= clock
    sortedArray_left_merged_swappedSeq_m_1.reset <= reset
    sortedArray_left_merged_swappedSeq_m_1.io_a0 <= _GEN_6
    sortedArray_left_merged_swappedSeq_m_1.io_a1 <= _GEN_7
    sortedArray_left_merged_left_swappedSeq_m.clock <= clock
    sortedArray_left_merged_left_swappedSeq_m.reset <= reset
    sortedArray_left_merged_left_swappedSeq_m.io_a0 <= _GEN_8
    sortedArray_left_merged_left_swappedSeq_m.io_a1 <= _GEN_9
    sortedArray_left_merged_right_swappedSeq_m.clock <= clock
    sortedArray_left_merged_right_swappedSeq_m.reset <= reset
    sortedArray_left_merged_right_swappedSeq_m.io_a0 <= _GEN_10
    sortedArray_left_merged_right_swappedSeq_m.io_a1 <= _GEN_11
    sortedArray_right_left_merged_swappedSeq_m.clock <= clock
    sortedArray_right_left_merged_swappedSeq_m.reset <= reset
    sortedArray_right_left_merged_swappedSeq_m.io_a0 <= _GEN_12
    sortedArray_right_left_merged_swappedSeq_m.io_a1 <= _GEN_13
    sortedArray_right_right_merged_swappedSeq_m.clock <= clock
    sortedArray_right_right_merged_swappedSeq_m.reset <= reset
    sortedArray_right_right_merged_swappedSeq_m.io_a0 <= _GEN_14
    sortedArray_right_right_merged_swappedSeq_m.io_a1 <= _GEN_15
    sortedArray_right_merged_swappedSeq_m.clock <= clock
    sortedArray_right_merged_swappedSeq_m.reset <= reset
    sortedArray_right_merged_swappedSeq_m.io_a0 <= _GEN_16
    sortedArray_right_merged_swappedSeq_m.io_a1 <= _GEN_17
    sortedArray_right_merged_swappedSeq_m_1.clock <= clock
    sortedArray_right_merged_swappedSeq_m_1.reset <= reset
    sortedArray_right_merged_swappedSeq_m_1.io_a0 <= _GEN_18
    sortedArray_right_merged_swappedSeq_m_1.io_a1 <= _GEN_19
    sortedArray_right_merged_left_swappedSeq_m.clock <= clock
    sortedArray_right_merged_left_swappedSeq_m.reset <= reset
    sortedArray_right_merged_left_swappedSeq_m.io_a0 <= _GEN_20
    sortedArray_right_merged_left_swappedSeq_m.io_a1 <= _GEN_21
    sortedArray_right_merged_right_swappedSeq_m.clock <= clock
    sortedArray_right_merged_right_swappedSeq_m.reset <= reset
    sortedArray_right_merged_right_swappedSeq_m.io_a0 <= _GEN_22
    sortedArray_right_merged_right_swappedSeq_m.io_a1 <= _GEN_23
    sortedArray_merged_swappedSeq_m.clock <= clock
    sortedArray_merged_swappedSeq_m.reset <= reset
    sortedArray_merged_swappedSeq_m.io_a0 <= _GEN_24
    sortedArray_merged_swappedSeq_m.io_a1 <= _GEN_25
    sortedArray_merged_swappedSeq_m_1.clock <= clock
    sortedArray_merged_swappedSeq_m_1.reset <= reset
    sortedArray_merged_swappedSeq_m_1.io_a0 <= _GEN_26
    sortedArray_merged_swappedSeq_m_1.io_a1 <= _GEN_27
    sortedArray_merged_swappedSeq_m_2.clock <= clock
    sortedArray_merged_swappedSeq_m_2.reset <= reset
    sortedArray_merged_swappedSeq_m_2.io_a0 <= _GEN_28
    sortedArray_merged_swappedSeq_m_2.io_a1 <= _GEN_29
    sortedArray_merged_swappedSeq_m_3.clock <= clock
    sortedArray_merged_swappedSeq_m_3.reset <= reset
    sortedArray_merged_swappedSeq_m_3.io_a0 <= _GEN_30
    sortedArray_merged_swappedSeq_m_3.io_a1 <= _GEN_31
    sortedArray_merged_left_swappedSeq_m.clock <= clock
    sortedArray_merged_left_swappedSeq_m.reset <= reset
    sortedArray_merged_left_swappedSeq_m.io_a0 <= _GEN_32
    sortedArray_merged_left_swappedSeq_m.io_a1 <= _GEN_33
    sortedArray_merged_left_swappedSeq_m_1.clock <= clock
    sortedArray_merged_left_swappedSeq_m_1.reset <= reset
    sortedArray_merged_left_swappedSeq_m_1.io_a0 <= _GEN_34
    sortedArray_merged_left_swappedSeq_m_1.io_a1 <= _GEN_35
    sortedArray_merged_left_left_swappedSeq_m.clock <= clock
    sortedArray_merged_left_left_swappedSeq_m.reset <= reset
    sortedArray_merged_left_left_swappedSeq_m.io_a0 <= _GEN_36
    sortedArray_merged_left_left_swappedSeq_m.io_a1 <= _GEN_37
    sortedArray_merged_left_right_swappedSeq_m.clock <= clock
    sortedArray_merged_left_right_swappedSeq_m.reset <= reset
    sortedArray_merged_left_right_swappedSeq_m.io_a0 <= _GEN_38
    sortedArray_merged_left_right_swappedSeq_m.io_a1 <= _GEN_39
    sortedArray_merged_right_swappedSeq_m.clock <= clock
    sortedArray_merged_right_swappedSeq_m.reset <= reset
    sortedArray_merged_right_swappedSeq_m.io_a0 <= _GEN_40
    sortedArray_merged_right_swappedSeq_m.io_a1 <= _GEN_41
    sortedArray_merged_right_swappedSeq_m_1.clock <= clock
    sortedArray_merged_right_swappedSeq_m_1.reset <= reset
    sortedArray_merged_right_swappedSeq_m_1.io_a0 <= _GEN_42
    sortedArray_merged_right_swappedSeq_m_1.io_a1 <= _GEN_43
    sortedArray_merged_right_left_swappedSeq_m.clock <= clock
    sortedArray_merged_right_left_swappedSeq_m.reset <= reset
    sortedArray_merged_right_left_swappedSeq_m.io_a0 <= _GEN_44
    sortedArray_merged_right_left_swappedSeq_m.io_a1 <= _GEN_45
    sortedArray_merged_right_right_swappedSeq_m.clock <= clock
    sortedArray_merged_right_right_swappedSeq_m.reset <= reset
    sortedArray_merged_right_right_swappedSeq_m.io_a0 <= _GEN_46
    sortedArray_merged_right_right_swappedSeq_m.io_a1 <= _GEN_47

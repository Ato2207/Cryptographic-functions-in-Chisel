FIRRTL version 1.2.0
circuit RSACore :
  module RSACore :
    input clock : Clock
    input reset : UInt<1>
    input io_start : UInt<1> @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 11:14]
    input io_base : UInt<16> @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 11:14]
    input io_exp : UInt<16> @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 11:14]
    input io_mod : UInt<16> @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 11:14]
    output io_done : UInt<1> @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 11:14]
    output io_result : UInt<16> @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 11:14]

    reg state : UInt<2>, clock with :
      reset => (UInt<1>("h0"), state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 24:34]
    reg resultReg : UInt<16>, clock with :
      reset => (UInt<1>("h0"), resultReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 26:36]
    reg baseReg : UInt<16>, clock with :
      reset => (UInt<1>("h0"), baseReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 27:36]
    reg expReg : UInt<16>, clock with :
      reset => (UInt<1>("h0"), expReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 28:36]
    node _io_done_T = eq(state, UInt<2>("h2")) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 31:23]
    node _T = asUInt(UInt<1>("h0")) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _T_1 = asUInt(state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _T_2 = eq(_T, _T_1) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _baseReg_T = rem(io_base, io_mod) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 37:30]
    node _GEN_0 = mux(io_start, UInt<1>("h1"), resultReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 26:36 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 35:22 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 36:19]
    node _GEN_1 = mux(io_start, _baseReg_T, baseReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 27:36 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 35:22 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 37:19]
    node _GEN_2 = mux(io_start, io_exp, expReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 28:36 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 35:22 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 38:19]
    node _GEN_3 = mux(io_start, UInt<1>("h1"), state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 24:34 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 35:22 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 39:19]
    node _T_3 = asUInt(UInt<1>("h1")) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _T_4 = asUInt(state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _T_5 = eq(_T_3, _T_4) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _doMul_T = bits(expReg, 0, 0) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 44:30]
    node doMul = eq(_doMul_T, UInt<1>("h1")) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 44:34]
    node _baseNext_T = mul(baseReg, baseReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 45:33]
    node baseNext = rem(_baseNext_T, io_mod) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 45:44]
    node _resultNext_T = mul(resultReg, baseReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 46:46]
    node _resultNext_T_1 = rem(_resultNext_T, io_mod) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 46:57]
    node resultNext = mux(doMul, _resultNext_T_1, resultReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 46:27]
    node expNext = shr(expReg, 1) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 47:31]
    node _T_6 = eq(expNext, UInt<1>("h0")) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 53:20]
    node _GEN_4 = mux(_T_6, UInt<2>("h2"), state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 24:34 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 53:29 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 54:15]
    node _T_7 = asUInt(UInt<2>("h2")) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _T_8 = asUInt(state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _T_9 = eq(_T_7, _T_8) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _T_10 = eq(io_start, UInt<1>("h0")) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 59:12]
    node _GEN_5 = mux(_T_10, UInt<1>("h0"), state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 24:34 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 59:23 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 60:15]
    node _GEN_6 = mux(_T_9, _GEN_5, state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 24:34 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _GEN_7 = mux(_T_5, resultNext, resultReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 26:36 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 49:17]
    node _GEN_8 = mux(_T_5, baseNext, baseReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 27:36 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 50:17]
    node _GEN_9 = mux(_T_5, expNext, expReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 28:36 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 51:17]
    node _GEN_10 = mux(_T_5, _GEN_4, _GEN_6) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _GEN_11 = mux(_T_2, _GEN_0, _GEN_7) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _GEN_12 = mux(_T_2, _GEN_1, _GEN_8) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _GEN_13 = mux(_T_2, _GEN_2, _GEN_9) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    node _GEN_14 = mux(_T_2, _GEN_3, _GEN_10) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:17]
    io_done <= _io_done_T @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 31:13]
    io_result <= resultReg @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:13]
    state <= mux(reset, UInt<1>("h0"), _GEN_14) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 24:34 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 24:34]
    resultReg <= _GEN_11
    baseReg <= _GEN_12
    expReg <= _GEN_13

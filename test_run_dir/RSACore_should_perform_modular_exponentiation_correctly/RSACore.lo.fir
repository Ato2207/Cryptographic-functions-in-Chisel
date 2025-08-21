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
      reset => (UInt<1>("h0"), state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 21:28]
    reg resultReg : UInt<16>, clock with :
      reset => (UInt<1>("h0"), resultReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 23:36]
    reg baseReg : UInt<16>, clock with :
      reset => (UInt<1>("h0"), baseReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 24:34]
    reg expReg : UInt<16>, clock with :
      reset => (UInt<1>("h0"), expReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 25:33]
    node _io_done_T = eq(state, UInt<2>("h2")) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 28:23]
    node _T = eq(UInt<2>("h0"), state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17]
    node _baseReg_T = rem(io_base, io_mod) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 34:30]
    node _GEN_0 = mux(io_start, UInt<1>("h1"), resultReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 23:36 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 32:22 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 33:19]
    node _GEN_1 = mux(io_start, _baseReg_T, baseReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 24:34 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 32:22 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 34:19]
    node _GEN_2 = mux(io_start, io_exp, expReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 25:33 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 32:22 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 35:19]
    node _GEN_3 = mux(io_start, UInt<2>("h1"), state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 21:28 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 32:22 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 36:19]
    node _T_1 = eq(UInt<2>("h1"), state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17]
    node _doMul_T = bits(expReg, 0, 0) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 41:25]
    node doMul = eq(_doMul_T, UInt<1>("h1")) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 41:29]
    node _baseNext_T = mul(baseReg, baseReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 42:31]
    node baseNext = rem(_baseNext_T, io_mod) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 42:42]
    node _resultNext_T = mul(resultReg, baseReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 43:46]
    node _resultNext_T_1 = rem(_resultNext_T, io_mod) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 43:57]
    node resultNext = mux(doMul, _resultNext_T_1, resultReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 43:27]
    node expNext = shr(expReg, 1) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 44:28]
    node _T_2 = eq(expNext, UInt<1>("h0")) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 50:20]
    node _GEN_4 = mux(_T_2, UInt<2>("h2"), state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 21:28 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 50:29 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 51:15]
    node _T_3 = eq(UInt<2>("h2"), state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17]
    node _T_4 = eq(io_start, UInt<1>("h0")) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 56:12]
    node _GEN_5 = mux(_T_4, UInt<2>("h0"), state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 21:28 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 56:23 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 57:15]
    node _GEN_6 = mux(_T_3, _GEN_5, state) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 21:28 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17]
    node _GEN_7 = mux(_T_1, resultNext, resultReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 23:36 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 46:17]
    node _GEN_8 = mux(_T_1, baseNext, baseReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 24:34 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 47:17]
    node _GEN_9 = mux(_T_1, expNext, expReg) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 25:33 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 48:17]
    node _GEN_10 = mux(_T_1, _GEN_4, _GEN_6) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17]
    node _GEN_11 = mux(_T, _GEN_0, _GEN_7) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17]
    node _GEN_12 = mux(_T, _GEN_1, _GEN_8) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17]
    node _GEN_13 = mux(_T, _GEN_2, _GEN_9) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17]
    node _GEN_14 = mux(_T, _GEN_3, _GEN_10) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 30:17]
    io_done <= _io_done_T @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 28:13]
    io_result <= resultReg @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 27:13]
    state <= mux(reset, UInt<2>("h0"), _GEN_14) @[D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 21:28 D:\\Scoala de vara\\Chisel\\Proiect\\src\\main\\scala\\rsa\\RSACore.scala 21:28]
    resultReg <= _GEN_11
    baseReg <= _GEN_12
    expReg <= _GEN_13

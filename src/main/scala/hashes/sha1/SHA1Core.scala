package hashes.sha1

import chisel3._
import chisel3.util._
import SHA1Consts._

class SHA1Core extends Module {
  val io = IO(new Bundle {
    val start: Bool          = Input(Bool())
    val block: Vec[UInt]     = Input(Vec(16, UInt(32.W)))   // one 512-bit block, BIG-endian words
    val initState: Vec[UInt] = Input(Vec(5, UInt(32.W)))    // chaining state (H0..H4)
    val done: Bool           = Output(Bool())
    val digest: Vec[UInt]    = Output(Vec(5, UInt(32.W)))   // output state (H0..H4)
  })

  // --- FSM state machine ---
  object State extends ChiselEnum {
    val idle, run, finalAdd, done = Value
  }
  import State._
  val state: State.Type = RegInit(idle)

  // --- Working registers ---
  val aReg, bReg, cReg, dReg, eReg      = Reg(UInt(32.W))
  val AInit, BInit, CInit, DInit, EInit = Reg(UInt(32.W))

  val roundIdx: UInt          = RegInit(0.U(7.W)) // 0..79
  private val wBuf: Vec[UInt] = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))

  // --- Outputs ---
  val doneReg: Bool        = RegInit(false.B)
  val digestReg: Vec[UInt] = RegInit(VecInit(Seq.fill(5)(0.U(32.W))))
  io.done   := doneReg
  io.digest := digestReg

  private def f(t: UInt, b: UInt, c: UInt, d: UInt): UInt =
    Mux(t < 20.U, (b & c) | ((~b).asUInt & d),
      Mux(t < 40.U, b ^ c ^ d,
        Mux(t < 60.U, (b & c) | (b & d) | (c & d),
          b ^ c ^ d)))

  private def Kconst(t: UInt): UInt =
    Mux(t < 20.U, K0,
      Mux(t < 40.U, K1,
        Mux(t < 60.U, K2, K3)))

  // --- FSM logic ---
  switch(state) {
    is(idle) {
      when(io.start) {
        // Load message words into buffer
        for (i <- 0 until 16) {
          wBuf(i) := io.block(i)
        }

        // Load chaining state
        aReg := io.initState(0)
        bReg := io.initState(1)
        cReg := io.initState(2)
        dReg := io.initState(3)
        eReg := io.initState(4)

        AInit := io.initState(0)
        BInit := io.initState(1)
        CInit := io.initState(2)
        DInit := io.initState(3)
        EInit := io.initState(4)

        roundIdx := 0.U
        doneReg  := false.B
        state    := run
      }
    }

    is(run) {
      // Generate W_t
      val t = roundIdx
      val w_t = Wire(UInt(32.W))
      when(t < 16.U) {
        w_t := wBuf(t)
      }.otherwise {
        val newWord = rol(wBuf(t - 3.U) ^ wBuf(t - 8.U) ^ wBuf(t - 14.U) ^ wBuf(t - 16.U), 1)
        w_t := newWord
        wBuf(t) := newWord
      }

      // Round step
      val temp = rol(aReg, 5) + f(t, bReg, cReg, dReg) + eReg + Kconst(t) + w_t
      eReg := dReg
      dReg := cReg
      cReg := rol(bReg, 30)
      bReg := aReg
      aReg := temp

      when(roundIdx === 79.U) {
        state := finalAdd
      }.otherwise {
        roundIdx := roundIdx + 1.U
      }
    }

    is(finalAdd) {
      val H0 = AInit + aReg
      val H1 = BInit + bReg
      val H2 = CInit + cReg
      val H3 = DInit + dReg
      val H4 = EInit + eReg

      digestReg := VecInit(Seq(H0, H1, H2, H3, H4))
      doneReg   := true.B
      state     := done
    }

    is(done) {
      doneReg := true.B

      when(!io.start) {
        state := idle
      }
    }
  }
}
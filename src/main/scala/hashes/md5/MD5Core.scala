package hashes.md5

import chisel3._
import chisel3.util._
import MD5Consts._

class MD5Core extends Module {
  val io = IO(new Bundle {
    val start: Bool          = Input(Bool())
    val block: Vec[UInt]     = Input(Vec(16, UInt(32.W))) // one 512-bit block
    val initState: Vec[UInt] = Input(Vec(4, UInt(32.W)))  // chaining state (A,B,C,D) for block
    val done: Bool           = Output(Bool())
    val digest: Vec[UInt]    = Output(Vec(4, UInt(32.W))) // output state (A,B,C,D)
  })

  // --- FSM state machine ---
  object State extends ChiselEnum {
    val idle, run, finalAdd, done = Value
  }
  import State._
  val state: State.Type = RegInit(idle)

  // --- Constants ---
  private val KVec = VecInit(K) // 64 constants
  private val SVec = VecInit(S.map(_.U(5.W))) // 64 shift amounts

  // --- Working registers ---
  val aReg, bReg, cReg, dReg             = Reg(UInt(32.W))
  private val AInit, BInit, CInit, DInit = Reg(UInt(32.W))

  private val roundIdx            = RegInit(0.U(7.W)) // 0..63
  private val msgWords: Vec[UInt] = io.block // 16 words

  // --- Outputs ---
  val doneReg: Bool        = RegInit(false.B)
  val digestReg: Vec[UInt] = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
  io.done   := doneReg
  io.digest := digestReg

  // --- FSM logic ---
  switch(state) {
    is(idle) {
      when(io.start) {
        // Load initial chaining state
        aReg := io.initState(0)
        bReg := io.initState(1)
        cReg := io.initState(2)
        dReg := io.initState(3)

        AInit := io.initState(0)
        BInit := io.initState(1)
        CInit := io.initState(2)
        DInit := io.initState(3)

        roundIdx := 0.U
        doneReg  := false.B
        state    := run
      }
    }

    is(run) {
      val f       = roundFunc(roundIdx, bReg, cReg, dReg)
      val kConst  = KVec(roundIdx)
      val sAmt    = SVec(roundIdx)
      val wordIdx = msgIndex(roundIdx)
      val msgWord = msgWords(wordIdx)

      val sum32   = aReg + f + kConst + msgWord
      val rotated = rol(sum32, sAmt)
      val newB    = bReg + rotated

      // Shuffle state
      aReg := dReg
      dReg := cReg
      cReg := bReg
      bReg := newB

      when(roundIdx === 63.U) {
        state := finalAdd
      }.otherwise {
        roundIdx := roundIdx + 1.U
      }
    }

    is(finalAdd) {
      val AFinal = AInit + aReg
      val BFinal = BInit + bReg
      val CFinal = CInit + cReg
      val DFinal = DInit + dReg

      digestReg := VecInit(Seq(AFinal, BFinal, CFinal, DFinal))
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
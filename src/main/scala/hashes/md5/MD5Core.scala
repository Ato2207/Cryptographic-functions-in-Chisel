package hashes.md5

import chisel3._
import chisel3.util._

class MD5Core extends Module {
  val io = IO(new Bundle {
    val start: Bool          = Input(Bool())
    val block: Vec[UInt]     = Input(Vec(16, UInt(32.W))) // one 512-bit block
    val initState: Vec[UInt] = Input(Vec(4, UInt(32.W)))  // chaining state (A,B,C,D) for block
    val done: Bool           = Output(Bool())
    val digest: Vec[UInt]    = Output(Vec(4, UInt(32.W))) // output state (A,B,C,D)
  })

  import MD5Consts._

  // --- FSM state machine ---
  object State extends ChiselEnum {
    val Idle, Run, FinalAdd, Done = Value
  }

  val state: State.Type = RegInit(State.Idle)

  // --- Constants ---
  private val KVec = VecInit(K) // 64 constants
  private val SVec = VecInit(S.map(_.U(5.W))) // 64 shift amounts

  // --- Working registers ---
  val aReg, bReg, cReg, dReg = Reg(UInt(32.W))
  private val AInit, BInit, CInit, DInit = Reg(UInt(32.W))

  private val roundIdx = RegInit(0.U(7.W)) // 0..63
  private val msgWords: Vec[UInt] = io.block // 16 words

  // --- Outputs ---
  val doneReg: Bool = RegInit(false.B)
  val digestReg: Vec[UInt] = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
  io.done := doneReg
  io.digest := digestReg

  // --- FSM logic ---
  switch(state) {
    is(State.Idle) {
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
        doneReg := false.B
        state := State.Run
      }
    }

    is(State.Run) {
      val f = roundFunc(roundIdx, bReg, cReg, dReg)
      val kConst = KVec(roundIdx)
      val sAmt = SVec(roundIdx)
      val wordIdx = msgIndex(roundIdx)
      val msgWord = msgWords(wordIdx)

      val sum32 = (aReg + f + kConst + msgWord)(31, 0)
      val rotated = rol32(sum32, sAmt)
      val newB = (bReg + rotated)(31, 0)

      // Shuffle state
      aReg := dReg
      dReg := cReg
      cReg := bReg
      bReg := newB

      when(roundIdx === 63.U) {
        state := State.FinalAdd
      }.otherwise {
        roundIdx := roundIdx + 1.U
      }
    }

    is(State.FinalAdd) {
      val AFinal = (AInit + aReg)(31, 0)
      val BFinal = (BInit + bReg)(31, 0)
      val CFinal = (CInit + cReg)(31, 0)
      val DFinal = (DInit + dReg)(31, 0)

      digestReg := VecInit(Seq(AFinal, BFinal, CFinal, DFinal))
      doneReg := true.B
      state := State.Done
    }

    is(State.Done) {
      doneReg := true.B
      when(!io.start) {
        state := State.Idle
      }
    }
  }
}
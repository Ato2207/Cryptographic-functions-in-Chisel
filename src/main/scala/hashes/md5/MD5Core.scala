package hashes.md5

import chisel3._
import chisel3.util._

// ---------- MD5Core ----------
class MD5CoreIO extends Bundle {
  val start = Input(Bool())

  // single block input: 16 x 32-bit words
  val block = Input(Vec(16, UInt(32.W)))
  val done  = Output(Bool())

  // final digest (A,B,C,D) little-endian 32-bit each
  val digest = Output(Vec(4, UInt(32.W)))
}

class MD5Core extends Module {
  val io = IO(new MD5CoreIO)

  import MD5Consts._
  val K_vec = VecInit(K) // converts Seq[UInt] -> Vec[UInt]
  val S_vec = VecInit(S.map(_.U(5.W))) // shift amounts fit in 5 bits

  // Initial state values (A,B,C,D)
  val A0 = "h67452301".U(32.W)
  val B0 = "hefcdab89".U(32.W)
  val C0 = "h98badcfe".U(32.W)
  val D0 = "h10325476".U(32.W)

  // working regs
  val a = RegInit(A0)
  val b = RegInit(B0)
  val c = RegInit(C0)
  val d = RegInit(D0)

  // saved initial for final addition
  val A_init = RegInit(A0)
  val B_init = RegInit(B0)
  val C_init = RegInit(C0)
  val D_init = RegInit(D0)

  val round = RegInit(0.U(7.W)) // 0..63
  val state = RegInit(0.U(2.W))
  val x = io.block // 16 words

  // ---------- NEW: output registers ----------
  val done_reg = RegInit(false.B)
  val digest_reg = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
  io.done := done_reg
  io.digest := digest_reg

  // helper rotate-left
  private def rol(x: UInt, s: UInt): UInt = {
    val left  = (x << s)(31, 0)
    val right = (x >> (32.U - s))(31, 0)
    (left | right)(31, 0).asUInt
  }

  // function F/G/H/I and index selection per-round
  private def F(i: UInt, B: UInt, C: UInt, D: UInt): UInt = {
    Mux(i < 16.U, (B & C) | ((~B).asUInt & D),
      Mux(i < 32.U, (D & B) | ((~D).asUInt & C),
        Mux(i < 48.U, B ^ C ^ D, C ^ (B | (~D).asUInt))
      )
    )
  }

  private def M_index(i: UInt): UInt = {
    Mux(i < 16.U, i,
      Mux(i < 32.U, (5.U * i + 1.U) % 16.U,
        Mux(i < 48.U, (3.U * i + 5.U) % 16.U,
          (7.U * i) % 16.U
        )
      )
    )
  }

  // Rounds FSM
  switch(state) {
    is(0.U) { // IDLE
      // clear done when a new run starts
      when(io.start) {
        // init working vars
        a := A0
        b := B0
        c := C0
        d := D0
        A_init := A0
        B_init := B0
        C_init := C0
        D_init := D0
        round := 0.U
        state := 1.U

        // clear outputs for new run
        done_reg := false.B
        // keep previous digest until new final value is written (or explicitly clear here if desired)
      }
    }
    is(1.U) { // RUNNING: one round per cycle
      val i = round
      val f = F(i, b, c, d)
      val k = K_vec(i)
      val s = S_vec(i)
      val mIdx = M_index(i)
      val mword = x(mIdx)

      // compute full sum then slice to 32b
      val sum32 = (a + f + k + mword)(31,0)
      val rotated = rol(sum32, s)
      val newB = (b + rotated)(31,0)

      // update in MD5 order:
      a := d
      d := c
      c := b
      b := newB

      when(round === 63.U) {
        state := 2.U
      }.otherwise {
        round := round + 1.U
      }
    }
    is(2.U) { // FINALIZE: add to initial state
      val A_final = (A_init + a)(31,0)
      val B_final = (B_init + b)(31,0)
      val C_final = (C_init + c)(31,0)
      val D_final = (D_init + d)(31,0)

      // write results into output registers (they stay stable)
      digest_reg := VecInit(Seq(A_final, B_final, C_final, D_final))
      done_reg := true.B

      state := 3.U
    }
    is(3.U) { // DONE: wait for start low (allow restarting)
      done_reg := true.B
      // keep digest_reg as-is; only clear when a new run starts (state 0 -> 1)
      when(!io.start) {
        state := 0.U
      }
    }
  }
}
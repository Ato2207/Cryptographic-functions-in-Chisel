package hashes.sha1

import chisel3._
import chisel3.util._
import SHA1Consts._

/** SHA-1 core: one round per cycle, processes a single 512-bit block.
 * On start, if initValid is true the core uses init as the initial H values,
 * otherwise it uses the standard IV constants. On completion it writes the new H
 * values to digest (i.e. H_out = H_in + computed_working_vars).
 */
class SHA1Core extends Module {
  val io = IO(new Bundle {
    val start     = Input(Bool())
    val block     = Input(Vec(16, UInt(32.W)))   // one 512-bit block, BIG-endian words

    // optional chaining inputs: if initValid true, core will initialize a..e from init on start
    val init      = Input(Vec(5, UInt(32.W)))
    val initValid = Input(Bool())

    val done      = Output(Bool())
    val digest    = Output(Vec(5, UInt(32.W)))   // H0..H4
  })

  // Initial state (H0..H4)
  val H0_0 = "h67452301".U(32.W)
  val H0_1 = "hefcdab89".U(32.W)
  val H0_2 = "h98badcfe".U(32.W)
  val H0_3 = "h10325476".U(32.W)
  val H0_4 = "hc3d2e1f0".U(32.W)

  // Working variables
  val a = RegInit(H0_0)
  val b = RegInit(H0_1)
  val c = RegInit(H0_2)
  val d = RegInit(H0_3)
  val e = RegInit(H0_4)

  // Keep initial (for final addition)
  val A_init = RegInit(H0_0)
  val B_init = RegInit(H0_1)
  val C_init = RegInit(H0_2)
  val D_init = RegInit(H0_3)
  val E_init = RegInit(H0_4)

  // Round counter and FSM
  val t = RegInit(0.U(7.W))     // 0..79
  val sIdle :: sRun :: sFinal :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  // Message schedule rolling buffer (16 words), on-the-fly W[t]
  val wbuf = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))

  // Output registers (digest stays until next run; done latched)
  val done_reg = RegInit(false.B)
  val digest_reg = RegInit(VecInit(Seq.fill(5)(0.U(32.W))))
  io.done := done_reg
  io.digest := digest_reg

  // Helpers
  private def rol(x: UInt, s: Int): UInt = {
    val sh = s.U(5.W)
    val left  = (x << sh)(31,0)
    val right = (x >> (32.U - sh))(31,0)
    (left | right)(31,0)
  }

  // Choose f(t, b, c, d) and K(t)
  private def f(t: UInt, b: UInt, c: UInt, d: UInt): UInt = {
    Mux(t < 20.U, (b & c) | ((~b).asUInt & d),
      Mux(t < 40.U, b ^ c ^ d,
        Mux(t < 60.U, (b & c) | (b & d) | (c & d), b ^ c ^ d)
      )
    )
  }

  private def Kconst(t: UInt): UInt = {
    Mux(t < 20.U, K0,
      Mux(t < 40.U, K1,
        Mux(t < 60.U, K2, K3)
      )
    )
  }

  // t mod 16
  val t_lo4 = t(3,0)

  // precompute extended-word formula (uses wbuf contents)
  val newWord = Wire(UInt(32.W))
  newWord := rol( wbuf((t_lo4 + 13.U) & 15.U) ^
    wbuf((t_lo4 + 8.U)  & 15.U) ^
    wbuf((t_lo4 + 2.U)  & 15.U) ^
    wbuf((t_lo4 + 0.U)  & 15.U), 1)

  // --- IMPORTANT: single combinational assignment for W_t (always driven) ---
  val W_t = Wire(UInt(32.W))
  W_t := Mux(t < 16.U, wbuf(t_lo4), newWord)

  // Rounds FSM
  switch(state) {
    is(sIdle) {
      when(io.start) {
        // load block words
        for (i <- 0 until 16) { wbuf(i) := io.block(i) }

        // initialize H_in and working registers either from init or IV
        when(io.initValid) {
          A_init := io.init(0); B_init := io.init(1); C_init := io.init(2)
          D_init := io.init(3); E_init := io.init(4)
          a := io.init(0); b := io.init(1); c := io.init(2); d := io.init(3); e := io.init(4)
        }.otherwise {
          A_init := H0_0; B_init := H0_1; C_init := H0_2; D_init := H0_3; E_init :=H0_4
          a := H0_0; b := H0_1; c := H0_2; d := H0_3; e := H0_4
        }

        t := 0.U
        state := sRun

        // clear outputs for a new run
        done_reg := false.B
      }
    }

    is(sRun) {
      // Compute one round using the combinational W_t
      val temp = (rol(a, 5) + f(t, b, c, d) + e + Kconst(t) + W_t)(31,0)

      // Update wbuf only when we used an extended word (t >= 16)
      when(t >= 16.U) { wbuf(t_lo4) := W_t }

      // Rotate variables
      e := d
      d := c
      c := rol(b, 30)
      b := a
      a := temp

      when(t === 79.U) {
        state := sFinal
      }.otherwise {
        t := t + 1.U
      }
    }

    is(sFinal) {
      // Final addition (mod 2^32)
      val H0 = (A_init + a)(31,0)
      val H1 = (B_init + b)(31,0)
      val H2 = (C_init + c)(31,0)
      val H3 = (D_init + d)(31,0)
      val H4 = (E_init + e)(31,0)

      // write results into output registers and set done
      digest_reg := VecInit(Seq(H0, H1, H2, H3, H4))
      done_reg := true.B

      state := sDone
    }

    is(sDone) {
      // Keep done_reg and digest_reg stable until a new run starts.
      done_reg := true.B
      when(!io.start) { state := sIdle } // allow restart after deasserting start
    }
  }
}

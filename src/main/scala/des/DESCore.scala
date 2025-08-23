package des

import chisel3._
import chisel3.util._
import des.DESConsts._
import des.DESConsts.rol

class DESCore extends Module {
  val io = IO(new Bundle {
    val start: Bool    = Input(Bool())
    val encrypt: Bool  = Input(Bool())           // true = encrypt, false = decrypt
    val key64: UInt    = Input(UInt(64.W))       // includes parity
    val inBlock: UInt  = Input(UInt(64.W))       // 64-bit block
    val done: Bool     = Output(Bool())
    val outBlock: UInt = Output(UInt(64.W))
  })

  // === FSM states ===
  object State extends ChiselEnum {
    val idle, keySched, init, round, finalOp, done = Value
  }
  import State._
  val state: State.Type = RegInit(idle)

  // === Registers ===
  private val subkeys = Reg(Vec(16, UInt(48.W)))
  private val rkIdx   = RegInit(0.U(5.W))
  val roundIdx: UInt  = RegInit(0.U(5.W))
  val C: UInt         = RegInit(0.U(28.W))
  val D: UInt         = RegInit(0.U(28.W))
  private val L       = RegInit(0.U(32.W))
  private val R       = RegInit(0.U(32.W))
  val doneReg: Bool   = RegInit(false.B)
  private val outReg  = RegInit(0.U(64.W))
  io.done     := doneReg
  io.outBlock := outReg

  // === Constants ===
  private val SBOX_VEC = SBOX.map(tbl => VecInit(tbl.map(v => (v & 0xf).U(4.W)))).toSeq
  private val ROT_VEC  = VecInit(ROT.map(_.U(5.W)))

  private def fFunc(r: UInt, k: UInt): UInt = {
    val x = permute(r, E) ^ k
    val sOut = VecInit((0 until 8).map { i =>
      val six = x(47 - 6*i, 42 - 6*i)
      SBOX_VEC(i)(Cat(six(5), six(0), six(4,1)))
    })

    permute(Cat(sOut.map(_.asUInt)), P)
  }

  private val start_p = io.start && !RegNext(io.start, false.B)

  // === FSM ===
  switch(state) {
    is(idle) {
      when(start_p) {
        doneReg := false.B
        val k56 = permute(io.key64, PC1)
        C := k56(55,28)
        D := k56(27,0)
        rkIdx := 0.U
        roundIdx := 0.U
        state := keySched
      }
    }

    is(keySched) {
      val rot = ROT_VEC(rkIdx)
      C := rol(C, rot)
      D := rol(D, rot)
      subkeys(rkIdx) := permute((C << 28).asUInt | D, PC2)

      when(rkIdx === 15.U) {
        state := init
      } .otherwise {
        rkIdx := rkIdx + 1.U
      }
    }

    is(init) {
      val ip = permute(io.inBlock, IP)
      L := ip(63,32)
      R := ip(31,0)
      roundIdx := 0.U
      state := round
    }

    is(round) {
      val idx = Mux(io.encrypt, roundIdx, 15.U - roundIdx)
      val f = fFunc(R, subkeys(idx))
      val (newL, newR) = (R, L ^ f)
      L := newL
      R := newR

      when(roundIdx === 15.U) {
        state := finalOp
      } .otherwise {
        roundIdx := roundIdx + 1.U
      }
    }

    is(finalOp) {
      outReg := permute(Cat(R, L), FP)
      doneReg := true.B
      state := done
    }

    is(done) {
      doneReg := true.B

      when(!io.start) {
        state := idle
      }
    }
  }
}
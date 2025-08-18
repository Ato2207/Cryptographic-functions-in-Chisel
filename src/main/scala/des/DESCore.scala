package des

import chisel3._
import chisel3.util._
import des.DESConsts._

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
  val sIdle :: sKeySched :: sInit :: sRound :: sFinal :: sDone :: Nil = Enum(6)
  val state: UInt = RegInit(sIdle)

  // === Registers ===
  private val subkeys = Reg(Vec(16, UInt(48.W)))
  private val rkIdx   = RegInit(0.U(5.W))
  val round: UInt     = RegInit(0.U(5.W))
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

  // === Helpers ===
  private def rol(x: UInt, sh: UInt, width: Int): UInt = {
    (x << sh)(width-1,0) | (x >> (width.U - sh))(width-1,0)
  }

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
    is(sIdle) {
      when(start_p) {
        doneReg := false.B
        val k56 = permute(io.key64, PC1)
        C := k56(55,28); D := k56(27,0)
        rkIdx := 0.U; round := 0.U
        state := sKeySched
      }
    }

    is(sKeySched) {
      val rot = ROT_VEC(rkIdx)
      C := rol(C, rot, 28)
      D := rol(D, rot, 28)
      subkeys(rkIdx) := permute((C << 28).asUInt | D, PC2)

      when(rkIdx === 15.U) {
        state := sInit
      } .otherwise {
        rkIdx := rkIdx + 1.U
      }
    }

    is(sInit) {
      val ip = permute(io.inBlock, IP)
      L := ip(63,32); R := ip(31,0); round := 0.U
      state := sRound
    }

    is(sRound) {
      val idx = Mux(io.encrypt, round, 15.U - round)
      val f = fFunc(R, subkeys(idx))
      val (newL, newR) = (R, L ^ f)
      L := newL; R := newR

      when(round === 15.U) {
        state := sFinal
      } .otherwise {
        round := round + 1.U
      }
    }

    is(sFinal) {
      outReg := permute(Cat(R, L), FP)
      doneReg := true.B
      state := sDone
    }

    is(sDone) {
      doneReg := true.B

      when(!io.start) {
        state := sIdle
      }
    }
  }
}

package des

import chisel3._
import chisel3.util._

import des.DESConsts._

class DESCore extends Module {
  val io = IO(new Bundle {
    val start    = Input(Bool())
    val encrypt  = Input(Bool())           // true = encrypt, false = decrypt
    val key64    = Input(UInt(64.W))       // includes parity; PC-1 will drop parity bits
    val inBlock  = Input(UInt(64.W))       // 64-bit data block (assembled big-endian from bytes)
    val done     = Output(Bool())
    val outBlock = Output(UInt(64.W))
  })

  // FSM states
  val sIdle :: sKeySched :: sInit :: sRound :: sFinal :: sDone :: Nil = Enum(6)
  val state = RegInit(sIdle)

  // Subkeys: 16 × 48-bit
  val subkeys = Reg(Vec(16, UInt(48.W)))
  val rkIdx   = RegInit(0.U(5.W)) // 0..15 for key schedule
  val round   = RegInit(0.U(5.W)) // 0..15 for Feistel rounds

  // Key schedule rolling C,D (28-bit each)
  val C = RegInit(0.U(28.W))
  val D = RegInit(0.U(28.W))

  // Data halves
  val L = RegInit(0.U(32.W))
  val R = RegInit(0.U(32.W))

  // Output regs
  val doneReg = RegInit(false.B)
  val outReg  = RegInit(0.U(64.W))
  io.done     := doneReg
  io.outBlock := outReg

  // === Hardware SBOX Vecs (so we can index with UInt) ===
  // DESConsts.SBOX should be Seq[Array[Int]] or Seq[Seq[Int]] with 8 tables of 64 ints
  val SBOX_VEC: Seq[Vec[UInt]] = SBOX.map { tbl =>
    // convert each scala table to VecInit of 4-bit UInts
    VecInit(tbl.map(v => (v & 0xf).U(4.W)).toSeq)
  }.toSeq

  // Rotation amounts vector for key schedule (use hardware indexing)
  val ROT_VEC = VecInit(ROT.map(_.U(5.W)).toSeq)

  // rotate-left with a UInt shift amount (hardware)
  private def rolVar(x: UInt, sh: UInt, width: Int): UInt = {
    val left  = (x << sh)(width-1, 0)
    val right = (x >> (width.U - sh))(width-1, 0)
    (left | right)(width-1, 0)
  }

  // f-function using hardware SBOX_VEC
  private def fFunc(r: UInt, k: UInt): UInt = {
    val exp = permute(r, E)                   // 32 -> 48
    val x   = exp ^ k                         // 48

    val sOut = Wire(Vec(8, UInt(4.W)))
    for (i <- 0 until 8) {
      val six = x(47 - i*6, 42 - i*6)        // 6 bits
      val row = Cat(six(5), six(0)).asUInt   // 2 bits
      val col = six(4,1).asUInt              // 4 bits
      val idx = Cat(row, col)                // 6-bit index
      sOut(i) := SBOX_VEC(i)(idx)            // hardware indexing
    }
    val cat32 = Cat(sOut.map(_.asUInt))
    permute(cat32, P)
  }

  // Start-edge detect
  val start_q = RegNext(io.start, false.B)
  val start_p = io.start && !start_q

  switch(state) {
    is (sIdle) {
      when (start_p) {
        doneReg := false.B
        rkIdx := 0.U
        round := 0.U

        // PC-1: 64->56
        val k56 = permute(io.key64, PC1)
        C := k56(55,28)
        D := k56(27,0)
        state := sKeySched
      }
    }

    is (sKeySched) {
      val rot = ROT_VEC(rkIdx)               // hardware shift
      val Cn  = rolVar(C, rot, 28)
      val Dn  = rolVar(D, rot, 28)
      val cd  = Cat(Cn, Dn)
      val k48 = permute(cd, PC2)
      subkeys(rkIdx) := k48
      C := Cn; D := Dn

      when (rkIdx === 15.U) { state := sInit }
        .otherwise { rkIdx := rkIdx + 1.U }
    }

    is (sInit) {
      val ip = permute(io.inBlock, IP)
      L := ip(63,32)
      R := ip(31,0)
      round := 0.U
      state := sRound
    }

    is (sRound) {
      val idx = Mux(io.encrypt, round, (15.U - round))
      val f = fFunc(R, subkeys(idx))
      val newL = R
      val newR = L ^ f
      L := newL
      R := newR

      when (round === 15.U) { state := sFinal }
        .otherwise { round := round + 1.U }
    }

    is (sFinal) {
      val preout = Cat(R, L) // swap
      outReg := permute(preout, FP)
      doneReg := true.B
      state := sDone
    }

    is (sDone) {
      doneReg := true.B
      when (!io.start) { state := sIdle }
    }
  }
}

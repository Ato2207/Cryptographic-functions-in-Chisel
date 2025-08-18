package des

import chisel3._
import chisel3.util._

class DESTop(val maxBytes: Int) extends Module {
  require(maxBytes >= 8, "DESTop requires maxBytes >= 8 (DES block is 8 bytes)")

  val io = IO(new Bundle {
    val start: Bool    = Input(Bool())
    val encrypt: Bool  = Input(Bool())                // true=encrypt, false=decrypt
    val key: UInt      = Input(UInt(64.W))
    val iv: UInt       = Input(UInt(64.W))
    val msg: Vec[UInt] = Input(Vec(maxBytes, UInt(8.W)))
    val msgLen: UInt   = Input(UInt(log2Ceil(maxBytes + 1).W))

    val done: Bool     = Output(Bool())
    val out: Vec[UInt] = Output(Vec(maxBytes + 8, UInt(8.W)))
    val outLen: UInt   = Output(UInt(log2Ceil(maxBytes + 9).W))
  })

  val core: DESCore = Module(new DESCore)

  // --- Start edge detect
  private val startPrev = RegNext(io.start, false.B)
  private val startEdge = io.start && !startPrev

  // --- Latched inputs (captured on startEdge)
  val mem: Vec[UInt]   = RegInit(VecInit(Seq.fill(maxBytes)(0.U(8.W))))
  val msgLenR: UInt    = RegInit(0.U(io.msgLen.getWidth.W))
  private val encryptR = RegInit(false.B)
  private val blocksR  = RegInit(0.U(16.W))

  // --- CBC / outputs
  private val blkIdx    = RegInit(0.U(16.W))
  private val prevCt    = RegInit(0.U(64.W))                         // previous ciphertext (C_{i-1}) or IV
  private val outBuf    = RegInit(VecInit(Seq.fill(maxBytes + 8)(0.U(8.W))))
  private val outLenR   = RegInit(0.U(io.outLen.getWidth.W))
  val doneR: Bool       = RegInit(false.B)
  private val lastPad   = RegInit(0.U(8.W))                          // candidate pad byte on decrypt

  io.done   := doneR
  io.out    := outBuf
  io.outLen := outLenR

  // --- helpers
  private def pack64(bytes: Seq[UInt]): UInt = bytes.reduceLeft((a, b) => Cat(a, b))
  private def unpack64(x: UInt): Vec[UInt] = VecInit((0 until 8).map(i => x(63 - 8 * i, 56 - 8 * i)))

  val rem_in: UInt = io.msgLen(2, 0)
  val full_in: UInt = (io.msgLen >> 3).asUInt
  private val totalBlocksEnc = full_in + 1.U
  private val totalBlocksDec = full_in
  val totalBlocks_in: UInt = Mux(io.encrypt, totalBlocksEnc, totalBlocksDec)

  // --- assemble block bytes
  val blockBase: UInt = blkIdx * 8.U
  private val blockBytes = Wire(Vec(8, UInt(8.W)))
  private val padVal = {
    val r = msgLenR(2, 0)
    Mux(r === 0.U, 8.U, 8.U - r)
  }

  private def plainByteAt(absIdx: UInt, pad: UInt): UInt = {
    val idxBits = log2Ceil(maxBytes)
    val idx = absIdx(idxBits - 1, 0)
    val inRange = absIdx < msgLenR
    val padByte = pad & 0xff.U(8.W)
    Mux(inRange, mem(idx), padByte)
  }

  private def cipherByteAt(absIdx: UInt): UInt = {
    val idxBits = log2Ceil(maxBytes)
    val idx = absIdx(idxBits - 1, 0)
    mem(idx)
  }

  when(encryptR) {
    for (i <- 0 until 8) blockBytes(i) := plainByteAt(blockBase + i.U, padVal)
  } .otherwise {
    for (i <- 0 until 8) blockBytes(i) := cipherByteAt(blockBase + i.U)
  }

  private val plainBlock64 = pack64((0 until 8).map(i => blockBytes(i)))
  private val coreIn = Wire(UInt(64.W))
  coreIn := Mux(encryptR, plainBlock64 ^ prevCt, plainBlock64)

  core.io.start := false.B // overridden by FSM
  core.io.encrypt := encryptR
  core.io.key64 := io.key
  core.io.inBlock := coreIn

  // --- FSM
  val sIdle :: sPrep :: sRun :: sWait :: sFinish :: sDone :: Nil = Enum(6)
  val state: UInt = RegInit(sIdle)

  // latch inputs when startEdge observed
  when(startEdge) {
    for (i <- 0 until maxBytes) mem(i) := io.msg(i)
    msgLenR := io.msgLen
    encryptR := io.encrypt
    blocksR := totalBlocks_in
    prevCt := io.iv
    blkIdx := 0.U
    outLenR := Mux(io.encrypt, totalBlocks_in * 8.U, io.msgLen)
    doneR := false.B
  }

  core.io.start := (state === sPrep)

  switch(state) {
    is(sIdle) {
      when(startEdge) { state := Mux(totalBlocks_in === 0.U, sFinish, sPrep) }
    }
    is(sPrep) { state := sRun } // core start asserted this cycle
    is(sRun)  { state := sWait } // let core run
    is(sWait) {
      when(core.io.done) {
        val y = core.io.outBlock
        val writeBase = (blkIdx << 3).asUInt

        when(encryptR) {
          val cbytes = unpack64(y)
          for (i <- 0 until 8) outBuf(writeBase + i.U) := cbytes(i)
          prevCt := y
          when(blkIdx === (blocksR - 1.U)) { state := sFinish }
            .otherwise { blkIdx := blkIdx + 1.U; state := sPrep }
        } .otherwise {
          val xored = y ^ prevCt
          val pbytes = unpack64(xored)
          for (i <- 0 until 8) outBuf(writeBase + i.U) := pbytes(i)

          // prevCt should become the ciphertext block we just fed to core (that is plainBlock64 when decrypting)
          prevCt := plainBlock64

          when(blkIdx === (blocksR - 1.U)) {
            lastPad := pbytes(7)
            state := sFinish
          } .otherwise {
            blkIdx := blkIdx + 1.U
            state := sPrep
          }
        }
      }
    }

    is(sFinish) {
      when(!encryptR && blocksR =/= 0.U) {
        val pad8 = lastPad(7, 0)
        val padOK = (pad8 >= 1.U) && (pad8 <= 8.U)
        val tbytes = blocksR * 8.U
        outLenR := Mux(padOK, (tbytes - pad8).asUInt, msgLenR)
      }
      doneR := true.B
      state := sDone
    }

    is(sDone) {
      doneR := true.B
      when(!io.start) { state := sIdle }
    }
  }
}
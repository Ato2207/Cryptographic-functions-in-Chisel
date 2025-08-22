package hashes.sha1

import chisel3._
import chisel3.util._

class SHA1Top(val maxBytes: Int) extends Module {
  val io = IO(new Bundle {
    val start: Bool       = Input(Bool())
    val msg: Vec[UInt]    = Input(Vec(maxBytes, UInt(8.W)))
    val msgLen: UInt      = Input(UInt(log2Ceil(maxBytes + 1).W))
    val done: Bool        = Output(Bool())
    val digest: Vec[UInt] = Output(Vec(5, UInt(32.W)))
  })

  val core: SHA1Core = Module(new SHA1Core)

  // ---------------- Internal state ----------------
  private val mem         = RegInit(VecInit(Seq.fill(maxBytes)(0.U(8.W))))
  private val msgLenReg   = RegInit(0.U(io.msgLen.getWidth.W))
  private val blockIdx    = RegInit(0.U(8.W))
  private val totalBlocks = RegInit(0.U(8.W))

  // SHA1 IV
  private val IV = Seq(
    "h67452301".U(32.W), "hefcdab89".U(32.W),
    "h98badcfe".U(32.W), "h10325476".U(32.W),
    "hc3d2e1f0".U(32.W)
  )
  private val chainState = RegInit(VecInit(IV))

  // Wire the core inputs
  private val coreWords = Wire(Vec(16, UInt(32.W)))
  core.io.block     := coreWords
  core.io.initState := chainState

  // Rising edge detect on external start
  private val startPrev  = RegNext(io.start, false.B)
  private val startPulse = io.start && !startPrev

  // ---------------- Block bookkeeping ----------------
  private val remBytes       = io.msgLen(5, 0)
  private val fullBlocks     = (io.msgLen >> 6).asUInt
  private val needsTwoBlocks = remBytes > 55.U
  private val totalBlocksIn  = fullBlocks + Mux(needsTwoBlocks, 2.U, 1.U)

  // Capture message & reset state on start
  when (startPulse) {
    for (i <- 0 until maxBytes) {
      mem(i) := 0.U
      when (i.U < io.msgLen) { mem(i) := io.msg(i) }
    }
    msgLenReg   := io.msgLen
    totalBlocks := totalBlocksIn
    blockIdx    := 0.U
    chainState  := VecInit(IV)
  }

  // ---------------- Message assembly with SHA-1 padding (big-endian length) ----------------
  private val msgLenBits = (msgLenReg.zext << 3).asUInt // length in bits
  private val lenBits64  = msgLenBits.pad(64) // pad to 64 bits and emit big-endian length bytes
  private val blockBase  = blockIdx * 64.U

  private def getByte(absIdx: UInt): UInt = {
    val inMsg        = absIdx < msgLenReg
    val isPadStart   = absIdx === msgLenReg
    val lenFieldBeg  = (totalBlocks * 64.U) - 8.U
    val inLenField   = absIdx >= lenFieldBeg

    val memIdxBits = log2Ceil(maxBytes)
    val memIdx     = absIdx(memIdxBits - 1, 0)

    // BIG-endian length bytes: most significant byte first
    val lenByteIdx = absIdx - lenFieldBeg     // 0..7
    val shift      = (7.U - lenByteIdx) * 8.U
    val lenByte    = (lenBits64 >> shift)(7,0)

    Mux(inMsg, mem(memIdx),
      Mux(isPadStart, 0x80.U(8.W),
        Mux(inLenField, lenByte, 0.U(8.W))
      )
    )
  }

  // Assemble 16 big-endian words
  for (w <- 0 until 16) {
    val base = blockBase + (w.U << 2).asUInt
    val b0 = getByte(base + 0.U)
    val b1 = getByte(base + 1.U)
    val b2 = getByte(base + 2.U)
    val b3 = getByte(base + 3.U)
    coreWords(w) := Cat(b0, b1, b2, b3) // big-endian word
  }

  // ---------------- Drive core (same pulse style as MD5Top) ----------------
  private val issueStart = RegInit(false.B)
  core.io.start := issueStart

  when (startPulse) {
    issueStart := true.B
  } .otherwise {
    issueStart := false.B
  }

  // detect core done rising edge
  private val coreDonePrev = RegNext(core.io.done, false.B)
  private val coreDoneRise = core.io.done && !coreDonePrev

  // ---------------- Outputs & chaining ----------------
  private val doneReg   = RegInit(false.B)
  private val digestReg = RegInit(VecInit(Seq.fill(5)(0.U(32.W))))
  io.done   := doneReg
  io.digest := digestReg

  when (coreDoneRise) {
    chainState := core.io.digest
    when (blockIdx === (totalBlocks - 1.U)) {
      digestReg := core.io.digest
      doneReg   := true.B
    } .otherwise {
      blockIdx   := blockIdx + 1.U
      issueStart := true.B // launch next block
    }
  }

  when (startPulse) {
    doneReg   := false.B
    digestReg := VecInit(Seq.fill(5)(0.U(32.W)))
  }
}
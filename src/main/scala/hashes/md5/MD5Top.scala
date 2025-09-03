package hashes.md5

import chisel3._
import chisel3.util._

class MD5Top(val maxBytes: Int) extends Module {
  val io = IO(new Bundle {
    val start: Bool       = Input(Bool())
    val msg: Vec[UInt]    = Input(Vec(maxBytes, UInt(8.W)))    // raw message
    val msgLen: UInt      = Input(UInt(log2Ceil(maxBytes + 1).W))
    val done: Bool        = Output(Bool())
    val digest: Vec[UInt] = Output(Vec(4, UInt(32.W)))         // final A,B,C,D
  })

  val core: MD5Core = Module(new MD5Core)

  // ---------------- Internal state ----------------
  val mem: Vec[UInt]            = RegInit(VecInit(Seq.fill(maxBytes)(0.U(8.W))))
  private val msgLenReg: UInt   = RegInit(0.U(io.msgLen.getWidth.W))
  val blockIdx: UInt            = RegInit(0.U(8.W))
  private val totalBlocks: UInt = RegInit(0.U(8.W))

  // MD5 initialization vector
  private val IV = Seq(
    "h67452301".U(32.W),
    "hefcdab89".U(32.W),
    "h98badcfe".U(32.W),
    "h10325476".U(32.W)
  )
  private val chainState: Vec[UInt] = RegInit(VecInit(IV))

  // Words assembled for core input
  private val coreWords: Vec[UInt] = Wire(Vec(16, UInt(32.W)))
  core.io.block     := coreWords
  core.io.initState := chainState

  // Rising-edge detect on external start
  val startPrev: Bool  = RegNext(io.start, false.B)
  val startPulse: Bool = io.start && !startPrev

  // ---------------- Block bookkeeping ----------------
  // Number of 512-bit blocks required (with padding+length)
  private val remBytes: UInt       = io.msgLen(5, 0)               // mod 64
  private val fullBlocks: UInt     = (io.msgLen >> 6).asUInt
  private val needsTwoBlocks: Bool = remBytes > 55.U
  private val totalBlocksIn: UInt  = fullBlocks + Mux(needsTwoBlocks, 2.U, 1.U)

  // Capture message & reset state on start
  when(startPulse) {
    for (i <- 0 until maxBytes) {
      mem(i) := 0.U
      when(i.U < io.msgLen) { mem(i) := io.msg(i) }
    }
    msgLenReg   := io.msgLen
    totalBlocks := totalBlocksIn
    blockIdx    := 0.U
    chainState  := VecInit(IV)
  }

  // ---------------- Message assembly with padding ----------------
  private val msgLenBits: UInt = (msgLenReg.zext << 3).asUInt
  val blockBase: UInt          = blockIdx * 64.U

  // Returns byte at absolute index in padded stream
  private def getByte(absIdx: UInt): UInt = {
    val inMsg       = absIdx < msgLenReg
    val isPadStart  = absIdx === msgLenReg
    val lenFieldBeg = (totalBlocks * 64.U) - 8.U
    val inLenField  = absIdx >= lenFieldBeg

    val memIdxBits  = log2Ceil(maxBytes)
    val memIdx      = absIdx(memIdxBits - 1, 0)
    val lenByteIdx  = absIdx - lenFieldBeg // 0..7
    val lenByte     = ((msgLenBits >> (lenByteIdx * 8.U)).asUInt & 0xff.U).asUInt

    Mux(inMsg, mem(memIdx),
      Mux(isPadStart, 0x80.U(8.W),
        Mux(inLenField, lenByte, 0.U(8.W))
      )
    )
  }

  // Pack 4 little-endian bytes into each word
  for (w <- 0 until 16) {
    val blockWordBase = blockBase + (w.U << 2).asUInt
    val b0 = getByte(blockWordBase + 0.U)
    val b1 = getByte(blockWordBase + 1.U)
    val b2 = getByte(blockWordBase + 2.U)
    val b3 = getByte(blockWordBase + 3.U)
    coreWords(w) := b0 | (b1 << 8).asUInt | (b2 << 16).asUInt | (b3 << 24).asUInt
  }

  // ---------------- Drive core ----------------
  private val issueStart: Bool = RegInit(false.B)
  core.io.start := issueStart

  when(startPulse) {
    issueStart := true.B
  } .otherwise {
    issueStart := false.B
  }

  // Rising edge detect on core.done
  private val coreDonePrev: Bool = RegNext(core.io.done, false.B)
  private val coreDoneRise: Bool = core.io.done && !coreDonePrev

  // ---------------- Outputs ----------------
  val doneReg: Bool        = RegInit(false.B)
  val digestReg: Vec[UInt] = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
  io.done       := doneReg
  io.digest     := digestReg

  // Update chaining state or finish
  when(coreDoneRise) {
    chainState := core.io.digest
    when(blockIdx === (totalBlocks - 1.U)) {
      digestReg := core.io.digest
      doneReg   := true.B
    }.otherwise {
      blockIdx   := blockIdx + 1.U
      issueStart := true.B
    }
  }

  // Reset outputs when new run begins
  when(startPulse) {
    doneReg   := false.B
    digestReg := VecInit(Seq.fill(4)(0.U(32.W)))
  }
}
package hashes.md5

import chisel3._
import chisel3.util._

class MD5Top(val maxBytes: Int) extends Module {
  require(maxBytes > 0)

  val io = IO(new Bundle {
    val start  = Input(Bool())
    val msg    = Input(Vec(maxBytes, UInt(8.W))) // caller should poke bytes before start
    val msgLen = Input(UInt(log2Ceil(maxBytes + 1).W)) // length in bytes
    val done   = Output(Bool())
    val digest = Output(Vec(4, UInt(32.W)))
  })

  val core = Module(new MD5Core)

  // --- internal memory / registers
  val mem = RegInit(VecInit(Seq.fill(maxBytes)(0.U(8.W))))
  val msgLenReg = RegInit(0.U(io.msgLen.getWidth.W))

  val blockIdx = RegInit(0.U(8.W))
  val totalBlocks = RegInit(0.U(8.W))

  // core block words (little-endian assembly)
  val coreWords = Wire(Vec(16, UInt(32.W)))
  core.io.block := coreWords

  // detect external rising-edge start
  val startReg = RegNext(io.start, false.B)
  val startPulse = io.start && !startReg

  // compute total blocks needed from the input length
  val rem_in = io.msgLen(5,0)
  val full_in = (io.msgLen >> 6).asUInt
  val finalBlocks_in = Mux(rem_in <= 56.U, 1.U, 2.U)
  val totalBlocks_in = (full_in + finalBlocks_in).asUInt

  // On start pulse capture message bytes & length
  when(startPulse) {
    for (i <- 0 until maxBytes) mem(i) := io.msg(i)
    msgLenReg := io.msgLen
    totalBlocks := totalBlocks_in
    blockIdx := 0.U
  }

  // length in bits (for appended length bytes)
  val lenBits = (msgLenReg.zext << 3).asUInt
  val blockBase = blockIdx * 64.U

  def getByteAtAbs(absIdx: UInt): UInt = {
    val inMsg = absIdx < msgLenReg
    val isPad80 = absIdx === msgLenReg
    val finalLenStart = (totalBlocks * 64.U) - 8.U
    val isLenByte = absIdx >= finalLenStart

    val memIdxBits = log2Ceil(maxBytes)
    val memIdx = absIdx(memIdxBits - 1, 0)
    val lenByteIdx = absIdx - finalLenStart
    val lenByte = ((lenBits >> (lenByteIdx * 8.U)).asUInt & 0xff.U).asUInt

    val fromMem = mem(memIdx)
    Mux(inMsg, fromMem,
      Mux(isPad80, 0x80.U(8.W),
        Mux(isLenByte, lenByte, 0.U(8.W))
      )
    )
  }

  for (w <- 0 until 16) {
    val b0 = getByteAtAbs(blockBase + (w.U * 4.U) + 0.U)
    val b1 = getByteAtAbs(blockBase + (w.U * 4.U) + 1.U)
    val b2 = getByteAtAbs(blockBase + (w.U * 4.U) + 2.U)
    val b3 = getByteAtAbs(blockBase + (w.U * 4.U) + 3.U)
    coreWords(w) := (b0.asUInt) | (b1.asUInt << 8).asUInt | (b2.asUInt << 16).asUInt | (b3.asUInt << 24).asUInt
  }

  // --- single-cycle start pulse to core
  val issueStart = RegInit(false.B)
  core.io.start := issueStart
  when(startPulse) { issueStart := true.B } .otherwise { issueStart := false.B }

  // compute rising-edge of core.done
  val prev_core_done = RegNext(core.io.done, false.B)
  val coreDoneRising = core.io.done && !prev_core_done

  // --- outputs
  val doneReg = RegInit(false.B)
  val digestReg = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
  io.done := doneReg
  io.digest := digestReg

  // reaction to core completion
  when(coreDoneRising) {
    digestReg := core.io.digest
    when(blockIdx === (totalBlocks - 1.U)) {
      doneReg := true.B
    } .otherwise {
      blockIdx := blockIdx + 1.U
      issueStart := true.B // single-cycle pulse for next block
    }
  }

  // clear digest / done when a new run begins
  when(startPulse) {
    doneReg := false.B
    digestReg := VecInit(Seq.fill(4)(0.U(32.W)))
    blockIdx := 0.U
  }
}
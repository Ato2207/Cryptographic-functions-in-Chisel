package hashes.sha1

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import chisel3.util._

class SHA1Top(val maxBytes: Int) extends Module {
  require(maxBytes > 0)

  val io = IO(new Bundle {
    val start  = Input(Bool())
    val msg    = Input(Vec(maxBytes, UInt(8.W))) // caller fills bytes before start
    val msgLen = Input(UInt(log2Ceil(maxBytes + 1).W)) // length in bytes
    val done   = Output(Bool())
    val digest = Output(Vec(5, UInt(32.W)))
  })

  val core = Module(new SHA1Core)

  // ===============================
  // Latch input message on start
  // ===============================
  val start_q    = RegNext(io.start, false.B)
  val startPulse = io.start && !start_q

  val mem     = RegInit(VecInit(Seq.fill(maxBytes)(0.U(8.W))))
  val msgLenR = RegInit(0.U(io.msgLen.getWidth.W))

  // number of 512-bit blocks to process (including padding)
  val blockIdx     = RegInit(0.U(16.W))
  val totalBlocksR = RegInit(0.U(16.W))

  when (startPulse) {
    for (i <- 0 until maxBytes) { mem(i) := io.msg(i) }
    msgLenR := io.msgLen
    blockIdx := 0.U

    // SHA-1 padding occupancy rule:
    // if (rem <= 55) need 1 block else 2, plus any full blocks before.
    val rem      = io.msgLen(5, 0)                 // % 64
    val full     = (io.msgLen >> 6).asUInt         // / 64
    val addBlks  = Mux(rem <= 55.U, 1.U, 2.U)
    totalBlocksR := (full + addBlks).asUInt
  }

  // ===============================
  // Build current block (big-endian)
  // ===============================
  // absolute byte index into the conceptual padded message stream
  val blockBase = blockIdx * 64.U  // 64 bytes per block

  // bit-length as 64-bit big-endian value of original message (in bits)
  private val lenShifted = (msgLenR << 3).asUInt
  private val lenBits    = Cat(0.U((64 - lenShifted.getWidth).W), lenShifted) // zero-extend to 64b

  // where the 8 length bytes start (absolute index)
  private val finalLenStart = (totalBlocksR * 64.U) - 8.U

  // helper: fetch abstract padded stream byte at absIdx
  def getByteAtAbs(absIdx: UInt): UInt = {
    val inMsg     = absIdx < msgLenR
    val isPad80   = absIdx === msgLenR
    val isLenByte = absIdx >= finalLenStart

    // choose from: message | 0x80 | zeros | length[8]
    val memIdxBits = log2Ceil(maxBytes)
    val memIdx     = absIdx(memIdxBits - 1, 0)

    val lenByteIdx = absIdx - finalLenStart // 0..7
    // big-endian length: most significant byte first
    val shift      = (7.U - lenByteIdx) * 8.U
    val lenByte    = (lenBits >> shift)(7,0)

    Mux(inMsg, mem(memIdx),
      Mux(isPad80, 0x80.U(8.W),
        Mux(isLenByte, lenByte, 0.U(8.W))
      )
    )
  }

  // Assemble 16×32-bit big-endian words for the current block
  val blockWords = Wire(Vec(16, UInt(32.W)))
  for (w <- 0 until 16) {
    val base = (w * 4).U
    val b0 = getByteAtAbs(blockBase + base + 0.U)
    val b1 = getByteAtAbs(blockBase + base + 1.U)
    val b2 = getByteAtAbs(blockBase + base + 2.U)
    val b3 = getByteAtAbs(blockBase + base + 3.U)
    // SHA-1 expects BIG-endian words
    blockWords(w) := Cat(b0, b1, b2, b3)
  }

  // ===============================
  // Drive the core
  // ===============================
  // Defaults (avoid FIRRTL init errors)
  core.io.block     := blockWords
  core.io.start     := false.B
  core.io.init      := VecInit(Seq.fill(5)(0.U(32.W)))
  core.io.initValid := false.B

  // We pass chaining state via init/initValid for subsequent blocks.
  val digestR = RegInit(VecInit(Seq.fill(5)(0.U(32.W))))
  val doneR   = RegInit(false.B)

  // On each block completion, capture the running digest.
  when (core.io.done) {
    digestR := core.io.digest
  }

  // For block 0: let core use its own IVs (initValid=false).
  // For block >0: initValid=true and init=digestR from previous block.
  val isFirstBlock = blockIdx === 0.U
  core.io.initValid := !isFirstBlock
  core.io.init      := digestR

  // ===============================
  // Simple FSM to step blocks
  // ===============================
  val sIdle :: sRun :: sWait :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  // one-cycle pulse into core at the start of each block
  val coreStart = RegInit(false.B)
  core.io.start := coreStart

  // clear done on new message
  when (startPulse) { doneR := false.B }

  switch (state) {
    is (sIdle) {
      when (startPulse) {
        // reset chaining reg so block 0 starts from the core IVs
        // (digestR value is ignored because initValid=false for first block)
        blockIdx := 0.U
        state    := sRun
      }
    }

    is (sRun) {
      // pulse start into the core for this block
      coreStart := true.B
      state     := sWait
    }

    is (sWait) {
      coreStart := false.B
      when (core.io.done) {
        when (blockIdx === (totalBlocksR - 1.U)) {
          // last block done
          doneR  := true.B
          state  := sDone
        } .otherwise {
          blockIdx := blockIdx + 1.U
          state    := sRun
        }
      }
    }

    is (sDone) {
      // hold done high until next start
      when (!io.start) {
        state := sIdle
      }
    }
  }

  io.done   := doneR
  io.digest := digestR
}
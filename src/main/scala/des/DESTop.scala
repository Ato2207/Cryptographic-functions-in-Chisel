package des

import chisel3._
import chisel3.util._

class DESTop(val maxBytes: Int) extends Module {
  require(maxBytes >= 8, "DESTop requires maxBytes >= 8 (DES block is 8 bytes)")

  val io = IO(new Bundle {
    val start     = Input(Bool())
    val encrypt   = Input(Bool())          // true=encrypt, false=decrypt
    val key       = Input(UInt(64.W))      // 64-bit key w/ parity bits
    val iv        = Input(UInt(64.W))      // 64-bit IV
    val msg       = Input(Vec(maxBytes, UInt(8.W)))
    val msgLen    = Input(UInt(log2Ceil(maxBytes + 1).W))

    val done      = Output(Bool())
    val out       = Output(Vec(maxBytes + 8, UInt(8.W))) // worst-case +8 for padding
    val outLen    = Output(UInt(log2Ceil(maxBytes + 9).W))

    val dbg_state      = Output(UInt(3.W))
    val dbg_core_mode  = Output(Bool())   // what core sees for encrypt
    val dbg_prevCBC    = Output(UInt(64.W))
    val dbg_lastPadReg = Output(UInt(8.W))
  })

  val core = Module(new DESCore)

  // Start pulse detection (rising edge)
  val start_q = RegNext(io.start, false.B)
  val start_p = io.start && !start_q

  // internal registers (latch inputs on start_p)
  val memIn   = RegInit(VecInit(Seq.fill(maxBytes)(0.U(8.W))))
  val lenR    = RegInit(0.U(io.msgLen.getWidth.W))
  val keyR    = RegInit(0.U(64.W))
  val ivR     = RegInit(0.U(64.W))
  val encR    = RegInit(false.B)

  // compute blocks/pad from input ports (combinational) — used on start_p
  val rem_in = io.msgLen(2,0)                // msgLen % 8
  val full_in = (io.msgLen >> 3).asUInt     // floor(msgLen / 8)
  val padVal_in = Mux(rem_in === 0.U, 8.U, (8.U - rem_in))   // 1..8
  val totalBlocksEnc_in = full_in + 1.U
  val totalBlocksDec_in = full_in
  val totalBlocks_in = Mux(io.encrypt, totalBlocksEnc_in, totalBlocksDec_in)

  // latch for totalBlocks (kept across FSM)
  val totalBlocks = RegInit(0.U(16.W))

  // utils: pack/unpack 8 bytes <-> 64-bit (big-endian)
  def pack64Vec(b: Vec[UInt]): UInt = {
    // b(0) -> bits [63..56], b(7) -> bits [7..0]
    Cat(b(0), b(1), b(2), b(3), b(4), b(5), b(6), b(7))
  }
  def unpack64(x: UInt): Vec[UInt] = {
    VecInit((0 until 8).map(i => x(63 - 8*i, 56 - 8*i)))
  }

  // helper to read plaintext/cipher bytes (plaintext includes PKCS#7 padding)
  // Note: this function reads regs (memIn, lenR, padVal) — it's intended to be used
  // once the registers have been updated (i.e. one cycle after start_p).
  def getPlainByte(absIdx: UInt, padVal: UInt): UInt = {
    val inMsg   = absIdx < lenR
    // make an 8-bit padding byte whose value = padVal (1..8)
    val padByte = (padVal & 0xff.U(8.W)) // zero-extend to 8 bits
    val memIdxBits = log2Ceil(maxBytes)
    val memIdx = absIdx(memIdxBits - 1, 0)
    Mux(inMsg, memIn(memIdx), padByte)
  }

  def getCipherByte(absIdx: UInt): UInt = {
    val memIdxBits = log2Ceil(maxBytes)
    val memIdx = absIdx(memIdxBits - 1, 0)
    memIn(memIdx)
  }

  // block index and chaining
  val blkIdx   = RegInit(0.U(16.W))
  val prevCBC  = RegInit(0.U(64.W))   // previous C_i (or IV)
  val outBuf   = RegInit(VecInit(Seq.fill(maxBytes + 8)(0.U(8.W))))
  val outLenR  = RegInit(0.U(io.outLen.getWidth.W))
  val doneR    = RegInit(false.B)
  val lastPadReg = RegInit(0.U(8.W))

  io.done   := doneR
  io.out    := outBuf
  io.outLen := outLenR

  // FSM
  val sIdle :: sPrep :: sRun :: sWait :: sFinish :: sDone :: Nil = Enum(6)
  val state = RegInit(sIdle)

  // default core hookups (will be overridden by combinational expressions)
  core.io.start   := false.B
  core.io.encrypt := io.encrypt
  core.io.key64   := io.key
  core.io.inBlock := 0.U

  // combinational assembly of current block (uses regs when processing; those regs
  // are guaranteed to have new values when state==sPrep because we latched them at start_p)
  val blockBase = blkIdx * 8.U
  val inBlock64 = WireInit(0.U(64.W))
  val eightBytes = Wire(Vec(8, UInt(8.W)))

  // padVal derived from reg lenR (zero-extended when used)
  val rem = lenR(2,0)
  val padVal = Mux(rem === 0.U, 8.U, (8.U - rem))

  when (encR) {
    for (i <- 0 until 8) { eightBytes(i) := getPlainByte(blockBase + i.U, padVal) }
    val plain64 = pack64Vec(eightBytes)
    inBlock64 := plain64 ^ prevCBC
  } .otherwise {
    for (i <- 0 until 8) { eightBytes(i) := getCipherByte(blockBase + i.U) }
    val cipher64 = pack64Vec(eightBytes)
    inBlock64 := cipher64
  }

  core.io.inBlock := inBlock64

  // pulse core on state == sPrep (one cycle)
  core.io.start := (state === sPrep)

  // FSM behavior and start_p handling
  when (start_p) {
    // latch inputs into regs
    for (i <- 0 until maxBytes) { memIn(i) := io.msg(i) }
    lenR := io.msgLen
    keyR := io.key
    ivR  := io.iv
    encR := io.encrypt

    // latch block counts and init CBC
    totalBlocks := totalBlocks_in
    prevCBC := io.iv
    blkIdx := 0.U

    // set output length immediately using combinational totalBlocks_in
    outLenR := Mux(io.encrypt, (totalBlocks_in * 8.U), io.msgLen)

    // prepare FSM (stay in sIdle until the main switch processes)
    // we set state inside the main switch below — but ensure it's set to start processing
  }

  switch (state) {
    is (sIdle) {
      when (start_p) {
        // decide whether there are blocks to process (use combinational totalBlocks_in)
        state := Mux(totalBlocks_in === 0.U, sFinish, sPrep)
        doneR := false.B
      }
    }

    is (sPrep) {
      // core.io.start is true for exactly this cycle (see above)
      state := sRun
    }

    is (sRun) {
      // deassert core start next cycle, move to wait state
      state := sWait
    }

    is (sWait) {
      when (core.io.done) {
        val y = core.io.outBlock
        val writeBase = (blkIdx << 3).asUInt // blkIdx * 8

        when (encR) {
          // Encryption: write ciphertext bytes and update CBC
          val cbytes = unpack64(y)
          for (i <- 0 until 8) {
            outBuf(writeBase + i.U) := cbytes(i)
          }
          prevCBC := y

          // next block or finish
          when (blkIdx === (totalBlocks - 1.U)) {
            state := sFinish
          } .otherwise {
            blkIdx := blkIdx + 1.U
            state := sPrep
          }
        } .otherwise {
          // Decryption: plaintext = D_k(C_i) XOR C_{i-1}
          val xored = y ^ prevCBC
          val pbytes = unpack64(xored)

          printf(
            p"DBG: io.encrypt=${io.encrypt} encR=${encR} core_encrypt=${core.io.encrypt} " +
              p"blkIdx=${blkIdx} y=${Hexadecimal(y)} prevCBC=${Hexadecimal(prevCBC)} " +
              p"xored=${Hexadecimal(xored)} p7=${Hexadecimal(pbytes(7))}\n"
          )


          for (i <- 0 until 8) {
            outBuf(writeBase + i.U) := pbytes(i)
          }
          // update prevCBC to the ciphertext block we consumed (eightBytes holds the input ciphertext bytes)
          prevCBC := pack64Vec(eightBytes)

          // If this was the last block, capture the last plaintext byte into a reg
          when (blkIdx === (totalBlocks - 1.U)) {
            lastPadReg := pbytes(7)  // candidate padding byte
            state := sFinish
          } .otherwise {
            blkIdx := blkIdx + 1.U
            state := sPrep
          }
        }
      }
    }

    is (sFinish) {
      // Compute final output length for decryption using the captured pad byte.
      // Use explicit widths for constants to avoid width-mismatch surprises.
      when (!encR && totalBlocks =/= 0.U) {
        val pad8   = lastPadReg(7,0)            // 8-bit pad candidate
        val one8   = 1.U(8.W)
        val eight8 = 8.U(8.W)
        val padOK  = (pad8 >= one8) && (pad8 <= eight8)
        // compute newLen = totalBlocks*8 - pad (all widened to outLenR width)
        val tbits = totalBlocks * 8.U
        val newLen = Mux(padOK, (tbits - pad8).asUInt, lenR)
        outLenR := newLen
      }
      // For encryption outLenR was set at start_p
      doneR := true.B
      state := sDone
    }

    is (sDone) {
      doneR := true.B
      when (!io.start) { state := sIdle }
    }
  }

  io.dbg_state      := state
  io.dbg_core_mode  := core.io.encrypt
  io.dbg_prevCBC    := prevCBC
  io.dbg_lastPadReg := lastPadReg
}

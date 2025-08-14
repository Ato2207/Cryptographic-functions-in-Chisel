package hashes.sha1

import chisel3._

/** Thin wrapper like your MD5Top: feeds a pre-padded single block to the core. */
class SHA1Top extends Module {
  val io = IO(new Bundle {
    val start  = Input(Bool())
    val block  = Input(Vec(16, UInt(32.W))) // one 512-bit padded block (BIG-endian words)
    val done   = Output(Bool())
    val digest = Output(Vec(5, UInt(32.W)))
  })

  val core = Module(new SHA1Core)
  core.io.start := io.start
  core.io.block := io.block

  io.done   := core.io.done
  io.digest := core.io.digest
}
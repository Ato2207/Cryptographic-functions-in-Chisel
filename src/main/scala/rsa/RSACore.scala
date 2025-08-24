package rsa

import chisel3._
import chisel3.util._

/*
 * RSACore: Performs modular exponentiation using square-and-multiply
 * result = base^exp mod mod.
 */
class RSACore(val modulusBits: Int) extends Module {
  val io = IO(new Bundle {
    val start: Bool  = Input(Bool())
    val base: UInt   = Input(UInt(modulusBits.W))
    val exp: UInt    = Input(UInt(modulusBits.W))
    val mod: UInt    = Input(UInt(modulusBits.W))
    val done: Bool   = Output(Bool())
    val result: UInt = Output(UInt(modulusBits.W))
  })

  object State extends ChiselEnum {
    val idle, run, done = Value
  }
  import State._
  val state: State.Type = RegInit(idle)

  private val resultReg: UInt = Reg(UInt(modulusBits.W))
  private val baseReg: UInt   = Reg(UInt(modulusBits.W))
  private val expReg: UInt    = Reg(UInt(modulusBits.W))

  io.result := resultReg
  io.done   := (state === done)

  switch(state) {
    is(idle) {
      when(io.start) {
        resultReg := 1.U
        baseReg   := io.base % io.mod
        expReg    := io.exp
        state     := run
      }
    }

    is(run) {
      val doMul      = expReg(0) === 1.U
      val baseNext   = (baseReg * baseReg) % io.mod
      val resultNext = Mux(doMul, (resultReg * baseReg) % io.mod, resultReg)
      val expNext    = expReg >> 1

      resultReg := resultNext
      baseReg   := baseNext
      expReg    := expNext

      when(expNext === 0.U) {
        state := done
      }
    }

    is(done) {
      when(!io.start) {
        state := idle
      }
    }
  }
}

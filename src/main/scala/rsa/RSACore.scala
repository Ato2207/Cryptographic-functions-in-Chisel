package rsa

import chisel3._
import chisel3.util._

/*
 * RSACore: Performs modular exponentiation using square-and-multiply
 * result = base^exp mod mod.
 */
class RSACore(val width: Int) extends Module {
  val io = IO(new Bundle {
    val start: Bool = Input(Bool())
    val base: UInt = Input(UInt(width.W))
    val exp: UInt = Input(UInt(width.W))
    val mod: UInt = Input(UInt(width.W))
    val done: Bool = Output(Bool())
    val result: UInt = Output(UInt(width.W))
  })

  val sIdle :: sRun :: sDone :: Nil = Enum(3)
  val state: UInt = RegInit(sIdle)

  private val resultReg: UInt = Reg(UInt(width.W))
  private val baseReg: UInt = Reg(UInt(width.W))
  private val expReg: UInt = Reg(UInt(width.W))

  io.result := resultReg
  io.done   := (state === sDone)

  switch(state) {
    is(sIdle) {
      when(io.start) {
        resultReg := 1.U
        baseReg   := io.base % io.mod
        expReg    := io.exp
        state     := sRun
      }
    }

    is(sRun) {
      val doMul = expReg(0) === 1.U
      val baseNext = (baseReg * baseReg) % io.mod
      val resultNext = Mux(doMul, (resultReg * baseReg) % io.mod, resultReg)
      val expNext = expReg >> 1

      resultReg := resultNext
      baseReg   := baseNext
      expReg    := expNext

      when(expNext === 0.U) {
        state := sDone
      }
    }

    is(sDone) {
      when(!io.start) {
        state := sIdle
      }
    }
  }
}

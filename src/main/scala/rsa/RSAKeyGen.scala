package rsa

import chisel3._
import chisel3.util._
import scala.util.Random

class RSAKeyGen(val modulusBits: Int, val genSeed: Int = 0xC0FFEE) extends Module {
  private val primeBits = modulusBits / 2

  val io = IO(new Bundle {
    val start: Bool = Input(Bool())
    val seed: UInt  = Input(UInt(32.W))
    val done: Bool  = Output(Bool())
    val n: UInt     = Output(UInt(modulusBits.W)) // modulus
    val e: UInt     = Output(UInt(32.W))          // public exponent
    val d: UInt     = Output(UInt(modulusBits.W)) // private exponent
  })

  // ------------------------
  // FSM States
  // ------------------------
  object State extends ChiselEnum {
    val idle, genP, genQ, computeN, chooseE, gcd, inv, done = Value
  }
  import State._
  val state: State.Type = RegInit(idle)

  // Rising-edge detection for start
  val startPrev: Bool  = RegNext(io.start, false.B)
  val startPulse: Bool = io.start && !startPrev

  // ------------------------
  // Registers
  // ------------------------
  private val pReg: UInt   = RegInit(0.U(primeBits.W))
  private val qReg: UInt   = RegInit(0.U(primeBits.W))
  private val nReg: UInt   = RegInit(0.U(modulusBits.W))
  private val phiReg: UInt = RegInit(0.U(modulusBits.W))
  private val eReg: UInt   = RegInit(0.U(32.W))
  private val dReg: UInt   = RegInit(1.U(modulusBits.W))
  val doneReg: Bool        = RegInit(false.B)

  io.n    := nReg
  io.e    := eReg
  io.d    := dReg
  io.done := doneReg

  // Candidate exponent and GCD registers
  private val eCandidate: UInt  = RegInit(3.U(32.W))
  private val gcdA: UInt        = RegInit(0.U(modulusBits.W))
  private val gcdB: UInt        = RegInit(0.U(modulusBits.W))
  private val phiMinusOne: UInt = Wire(UInt(modulusBits.W))
  phiMinusOne := Mux(phiReg === 0.U, 0.U, phiReg - 1.U)

  // Extended Euclidean Algorithm registers
  private val rReg: UInt    = Reg(UInt(modulusBits.W))
  private val newRReg: UInt = Reg(UInt(modulusBits.W))
  private val tReg: SInt    = Reg(SInt((modulusBits + 1).W))
  private val newTReg: SInt = Reg(SInt((modulusBits + 1).W))

  // ------------------------
  // Prime number generation (software helpers)
  // ------------------------
  private val rng = new Random(genSeed)

  private def nextOdd(x: BigInt): BigInt = if ((x & 1) == 1) x else x + 1

  private def isProbablePrime(n: BigInt, rounds: Int = 16): Boolean = {
    if (n < 2) return false
    if (n == 2 || n == 3) return true
    if ((n & 1) == 0) return false

    val nMinus1 = n - 1
    val r       = nMinus1.lowestSetBit
    val d       = nMinus1 >> r
    val rndLocal = new Random(n.hashCode() ^ genSeed)

    def trial(a: BigInt): Boolean = {
      var x = a.modPow(d, n)
      if (x == 1 || x == n - 1) return true
      for (_ <- 1 until r) {
        x = (x * x) % n
        if (x == n - 1) return true
      }
      false
    }

    (0 until rounds).forall { _ =>
      val a = (BigInt(primeBits, rndLocal) % (n - 3)) + 2
      trial(a)
    }
  }

  private def generatePrime(bits: Int): BigInt = {
    var candidate = nextOdd(BigInt(bits, rng).setBit(bits - 1)) // ensure odd & MSB = 1
    while (!isProbablePrime(candidate)) { candidate += 2 }
    candidate
  }

  // ------------------------
  // FSM Logic
  // ------------------------
  switch(state) {
    is(idle) {
      doneReg := false.B

      when(startPulse) {
        state := genP
      }
    }

    is(genP) {
      val p = generatePrime(primeBits)
      pReg  := p.U(primeBits.W)
      state := genQ
    }

    is(genQ) {
      val q = generatePrime(primeBits)
      qReg  := q.U(primeBits.W)
      state := computeN
    }

    is(computeN) {
      val pBig = pReg.zext
      val qBig = qReg.zext
      nReg   := (pBig * qBig).asUInt.pad(modulusBits)
      phiReg := ((pBig - 1.S) * (qBig - 1.S)).asUInt.pad(modulusBits)

      val seedOdd = Cat(io.seed(31,1), 1.U(1.W)) // ensure odd
      val startE  = Mux(seedOdd < 3.U, 3.U, seedOdd)
      eCandidate := Mux(startE >= phiMinusOne, 3.U, startE(31,0))
      state      := chooseE
    }

    is(chooseE) {
      gcdA  := phiReg
      gcdB  := eCandidate.pad(modulusBits)
      state := gcd
    }

    is(gcd) {
      when(gcdB === 0.U) {
        when(gcdA === 1.U) {
          // Found valid e
          eReg    := eCandidate
          rReg    := phiReg
          newRReg := eCandidate
          tReg    := 0.S
          newTReg := 1.S
          state   := inv
        } .otherwise {
          // Try next candidate e
          val nextE = eCandidate + 2.U
          eCandidate := Mux(nextE >= phiMinusOne, 3.U, nextE)
          state := chooseE
        }
      } .otherwise {
        val remainder = (gcdA % gcdB).asUInt
        gcdA := gcdB
        gcdB := remainder
      }
    }

    is(inv) {
      when(newRReg === 0.U) {
        state := idle // should not happen
      } .elsewhen(newRReg === 1.U) {
        val dPositive = Mux(newTReg < 0.S, newTReg + phiReg.asSInt, newTReg)
        dReg  := dPositive.asUInt
        state := done
      } .otherwise {
        val quotient = rReg / newRReg
        val tmpR     = rReg - quotient * newRReg
        rReg    := newRReg
        newRReg := tmpR

        val tmpT = tReg - (quotient.asSInt * newTReg)
        tReg    := newTReg
        newTReg := tmpT
      }
    }

    is(done) {
      doneReg := true.B

      when(!io.start) {
        state := idle
      }
    }
  }
}

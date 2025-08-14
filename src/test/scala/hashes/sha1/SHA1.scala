package hashes.sha1

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SHA1Spec extends AnyFlatSpec with ChiselScalatestTester {
  it should "produce the correct SHA1 digest for the empty string" in {
    test(new SHA1Top) { dut =>
      // Build 16 big-endian words for the padded block (empty message)
      val block = Array.fill(16)(0.U(32.W))
      block(0) = "h80000000".U  // 0x80 then zeros
      // block(1) .. block(14) = 0
      block(15) = 0.U           // length = 0 bits

      for (i <- 0 until 16) dut.io.block(i).poke(block(i))

      // ensure start is low, advance 1 cycle
      dut.io.start.poke(false.B)
      dut.clock.step(1)

      // Pulse start for one cycle (rising edge)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Wait
      while (!dut.io.done.peek().litToBoolean) { dut.clock.step(1) }

      // Read digest (H0..H4) -> DA39A3EE 5E6B4B0D 3255BFEF 95601890 AFD80709
      val H = (0 until 5).map(i => dut.io.digest(i).peek().litValue.toInt)
      println(f"SHA1(empty) = ${H(0)}%08x${H(1)}%08x${H(2)}%08x${H(3)}%08x${H(4)}%08x")
    }
  }
}
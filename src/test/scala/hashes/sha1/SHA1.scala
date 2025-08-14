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

      // Expected SHA1(empty) = DA39A3EE 5E6B4B0D 3255BFEF 95601890 AFD80709
      dut.io.digest(0).expect("hDA39A3EE".U)
      dut.io.digest(1).expect("h5E6B4B0D".U)
      dut.io.digest(2).expect("h3255BFEF".U)
      dut.io.digest(3).expect("h95601890".U)
      dut.io.digest(4).expect("hAFD80709".U)
    }
  }

  it should "produce the correct SHA1 digest for the string 'abc'" in {
    test(new SHA1Top) { dut =>
      // Build 16 big-endian words for the padded block (message "abc")
      val block = Array.fill(16)(0.U(32.W))
      block(0) = "h61626380".U  // ASCII for "abc" followed by 0x80
      // block(1) .. block(14) = 0
      block(15) = "h00000018".U // length = 24 bits (3 bytes)

      for (i <- 0 until 16) dut.io.block(i).poke(block(i))

      // Ensure start is low, advance 1 cycle
      dut.io.start.poke(false.B)
      dut.clock.step(1)

      // Pulse start for one cycle (rising edge)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Wait for completion
      while (!dut.io.done.peek().litToBoolean) { dut.clock.step(1) }

      // Read digest (H0..H4) -> A9993E36 4706816A BA3E2571 7850C26C 9CD0D89D
      dut.io.digest(0).expect("hA9993E36".U)
      dut.io.digest(1).expect("h4706816A".U)
      dut.io.digest(2).expect("hBA3E2571".U)
      dut.io.digest(3).expect("h7850C26C".U)
      dut.io.digest(4).expect("h9CD0D89D".U)
    }
  }
}
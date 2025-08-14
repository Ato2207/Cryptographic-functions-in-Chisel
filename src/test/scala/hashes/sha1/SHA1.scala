package hashes.sha1

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SHA1Spec extends AnyFlatSpec with ChiselScalatestTester {
  it should "produce the correct SHA1 digest for the empty string" in {
    test(new SHA1Top(64)) { dut =>
      for (i <- 0 until 64) dut.io.msg(i).poke(0.U)
      dut.io.msgLen.poke(0.U)

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
    test(new SHA1Top(64)) { dut =>
      // Prepare the input message "abc"
      val message = "abc".getBytes("UTF-8")
      for (i <- message.indices) {
        dut.io.msg(i).poke(message(i).U)
      }
      for (i <- message.length until 64) {
        dut.io.msg(i).poke(0.U) // Fill remaining bytes with zeros
      }
      dut.io.msgLen.poke(message.length.U) // Set the message length

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

  it should "produce the correct SHA1 digest for a string longer than 512 bits" in {
    // Use a larger maxBytes to accommodate the input
    test(new SHA1Top(128)) { dut =>
      // Prepare the input message: "a" repeated 100 times
      val message = "a" * 100
      val messageBytes = message.getBytes("UTF-8")
      for (i <- messageBytes.indices) {
        dut.io.msg(i).poke(messageBytes(i).U)
      }
      for (i <- messageBytes.length until 128) {
        dut.io.msg(i).poke(0.U) // Fill remaining bytes with zeros
      }
      dut.io.msgLen.poke(messageBytes.length.U) // Set the message length

      // Ensure start is low, advance 1 cycle
      dut.io.start.poke(false.B)
      dut.clock.step(1)

      // Pulse start for one cycle (rising edge)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Wait for completion
      while (!dut.io.done.peek().litToBoolean) { dut.clock.step(1) }

      // Expected SHA1("a" * 100) = 34AA973C D4C4DAA4 F61EEB2B DBAD2731 6534016F
      dut.io.digest(0).expect("h34AA973C".U)
      dut.io.digest(1).expect("hD4C4DAA4".U)
      dut.io.digest(2).expect("hF61EEB2B".U)
      dut.io.digest(3).expect("hDBAD2731".U)
      dut.io.digest(4).expect("h6534016F".U)
    }
  }
}
package des

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DESSpec extends AnyFlatSpec with ChiselScalatestTester {
  def runDESTest(keyHex: String, ivHex: String, plaintext: String, maxBytes: Int): Unit = {
    test(new DESTop(maxBytes)) { dut =>
      val key = BigInt(keyHex, 16)
      val iv  = BigInt(ivHex, 16)
      val msg = plaintext.getBytes("ASCII")

      // --- Load key and IV ---
      dut.io.key.poke(key)
      dut.io.iv.poke(iv)

      // --- Encrypt ---
      for (i <- msg.indices) {
        dut.io.msg(i).poke(msg(i).U)
      }
      dut.io.msgLen.poke(msg.length.U)

      dut.io.encrypt.poke(true.B)

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      while (!dut.io.done.peek().litToBoolean) {
        dut.clock.step(1)
      }

      val encLen = dut.io.outLen.peek().litValue.toInt
      val cipher = (0 until encLen).map(i => dut.io.out(i).peek().litValue.toByte).toArray

      // --- Reset FSM to idle ---
      dut.clock.step(1)

      // --- Decrypt ---
      for (i <- cipher.indices) {
        dut.io.msg(i).poke((cipher(i) & 0xff).U)
      }
      dut.io.msgLen.poke(cipher.length.U)

      dut.io.encrypt.poke(false.B)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      while (!dut.io.done.peek().litToBoolean) {
        dut.clock.step(1)
      }

      val decLen = dut.io.outLen.peek().litValue.toInt
      val dec = (0 until decLen).map(i => dut.io.out(i).peek().litValue.toByte).toArray

      // --- Assertions ---
      assert(decLen == msg.length, "Decrypted length mismatch")
      assert(dec.sameElements(msg), "Decrypted plaintext does not match original")
    }
  }

  "DESTop" should "encrypt/decrypt a short string" in {
    runDESTest("133457799BBCDFF1", "0123456789ABCDEF", "abc", 64)
  }

  "DESTop" should "encrypt/decrypt a 40 bytes string" in {
    runDESTest("539152739FFFE253", "0123456789ABCDEF", "cb" * 20, 64)
  }

  "DESTop" should "encrypt/decrypt a 60 bytes string" in {
    runDESTest("539152739FFFE253", "9876543210FEDCBA", "M23" * 20, 128)
  }

  "DESTop" should "encrypt/decrypt a 100 bytes string" in {
    runDESTest("302830239CDAB798", "23195BFFF985C11A", "P0KX" * 25, 128)
  }
}

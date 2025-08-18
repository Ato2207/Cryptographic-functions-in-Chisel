package des

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DESSpec extends AnyFlatSpec with ChiselScalatestTester {
  // Helper: load a message array into DUT
  private def loadMsg(dut: DESTop, msg: Array[Byte]): Unit = {
    for (i <- 0 until dut.maxBytes) {
      dut.io.msg(i).poke(if (i < msg.length) (msg(i) & 0xff).U else 0.U)
    }
    dut.io.msgLen.poke(msg.length.U)
  }

  // Helper: start the operation (encrypt/decrypt) and wait for done
  private def runOp(dut: DESTop, encrypt: Boolean): Unit = {
    dut.io.encrypt.poke(encrypt.B)
    dut.io.start.poke(true.B)
    dut.clock.step(1)
    dut.io.start.poke(false.B)
    while (!dut.io.done.peek().litToBoolean) dut.clock.step(1)
  }

  // Helper: ensure FSM back to idle
  private def resetFSM(dut: DESTop): Unit = {
    dut.io.start.poke(false.B)
    dut.clock.step(1)
  }

  // Helper: read back the output as bytes
  private def readOut(dut: DESTop, outLen: Int): Array[Byte] = {
    (0 until outLen).map(i => dut.io.out(i).peek().litValue.toByte).toArray
  }

  "DESTop" should "encrypt and decrypt a short string" in {
    test(new DESTop(maxBytes = 64)) { dut =>
      val key = BigInt("133457799BBCDFF1", 16)
      val iv  = BigInt("0123456789ABCDEF", 16)
      val msg = "abc".getBytes("ASCII")

      dut.io.key.poke(key)
      dut.io.iv.poke(iv)

      // Encrypt
      loadMsg(dut, msg)
      runOp(dut, encrypt = true)
      val encLen = dut.io.outLen.peek().litValue.toInt
      val cipher = readOut(dut, encLen)
      assert(encLen == 8, "Expected 8 bytes of ciphertext for 3-byte input")

      // Reset FSM to idle
      resetFSM(dut)

      // Decrypt
      loadMsg(dut, cipher)
      runOp(dut, encrypt = false)
      val decLen = dut.io.outLen.peek().litValue.toInt
      val dec = readOut(dut, decLen)
      assert(decLen == msg.length, "Decrypted length mismatch")
      assert(dec.sameElements(msg), "Decrypted plaintext does not match original")
    }
  }

  it should "encrypt and decrypt a long string in CBC mode" in {
    test(new DESTop(maxBytes = 128)) { dut =>
      val key = BigInt("133457799BBCDFF1", 16)
      val iv  = BigInt("0123456789ABCDEF", 16)
      val plaintext = "The quick brown fox jumps over the lazy dog"
      val msg = plaintext.getBytes("ASCII")

      dut.io.key.poke(key)
      dut.io.iv.poke(iv)

      // Encrypt
      loadMsg(dut, msg)
      runOp(dut, encrypt = true)
      val encLen = dut.io.outLen.peek().litValue.toInt
      val cipher = readOut(dut, encLen)

      // Reset FSM to idle
      resetFSM(dut)

      // Decrypt
      loadMsg(dut, cipher)
      runOp(dut, encrypt = false)
      val decLen = dut.io.outLen.peek().litValue.toInt
      val dec = readOut(dut, decLen)

      assert(decLen == msg.length, "Decrypted length mismatch")
      assert(dec.sameElements(msg), "Decrypted plaintext does not match original")
    }
  }
}

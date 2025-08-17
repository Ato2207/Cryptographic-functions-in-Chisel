package des

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DESSpec extends AnyFlatSpec with ChiselScalatestTester {
  it should "encrypt and decrypt the string abc" in {
    test(new DESTop(maxBytes = 64)) { dut =>
      // ---- test constants (classic DES test key + a sample IV) ----
      val keyVal = BigInt("133457799BBCDFF1", 16)  // 0x133457799BBCDFF1
      val ivVal  = BigInt("0123456789ABCDEF", 16)  // 0x0123456789ABCDEF

      // poke key and iv (they are UInt(64.W))
      dut.io.key.poke(keyVal)
      dut.io.iv.poke(ivVal)

      // Input = "abc" (ASCII 0x61,0x62,0x63), length = 3
      val msg = Array[Byte](0x61, 0x62, 0x63)
      for (i <- 0 until 64) {
        val v = if (i < msg.length) msg(i) & 0xff else 0
        dut.io.msg(i).poke(v.U)
      }
      dut.io.msgLen.poke(msg.length.U)

      // Ensure start is low initially
      dut.io.start.poke(false.B)
      dut.io.encrypt.poke(true.B) // encryption mode
      dut.clock.step(1)

      // Pulse start to begin encryption
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Wait until done asserted
      while (!dut.io.done.peek().litToBoolean) {
        dut.clock.step(1)
      }

      val encOutLen = dut.io.outLen.peek().litValue.intValue
      println(s"Encrypt done. outLen = $encOutLen bytes")

      // Read out ciphertext bytes (only up to outLen)
      val cipherBytes = (0 until encOutLen).map { i =>
        (dut.io.out(i).peek().litValue.intValue & 0xff)
      }.toArray

      println("Ciphertext (hex): " + cipherBytes.map(b => f"$b%02x").mkString(" "))

      // Expect encryption of "abc" (3 bytes) to produce 1 padded block (8 bytes)
      assert(encOutLen == 8, s"expected 8 bytes of ciphertext for input 'abc', got $encOutLen")

      // ---- Now decrypt: load ciphertext into msg, set msgLen = encOutLen ----
      for (i <- 0 until 64) {
        val v = if (i < encOutLen) cipherBytes(i) else 0
        dut.io.msg(i).poke((v & 0xff).U)
      }
      dut.io.msgLen.poke(encOutLen.U)

      // Set decrypt mode and pulse start
      dut.io.encrypt.poke(false.B)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Wait for done
      while (!dut.io.done.peek().litToBoolean) {
        dut.clock.step(1)
      }

      // peek debug signals
      val st  = dut.io.dbg_state.peek().litValue.intValue
      val cm  = dut.io.dbg_core_mode.peek().litToBoolean
      val prev = (0 until 8).map(i => (dut.io.dbg_prevCBC.peek().litValue.toLong >> (56 - 8*i) & 0xff).toInt)
      val lp = dut.io.dbg_lastPadReg.peek().litValue.intValue

      println(s"DBG: state=$st core_mode_encrypt=$cm prevCBC(bytes)=${prev.map(b => f"$b%02x").mkString(" ")} lastPadReg=0x${lp.toHexString}")

      // After decryption finished:
      val fullOut = (0 until 8).map { i => (dut.io.out(i).peek().litValue.intValue & 0xff) }
      println("decrypted first 8 bytes (hex): " + fullOut.map(b => f"$b%02x").mkString(" "))

      // peek the last byte of the first 8-bytes block (candidate pad)
      val lastByte = dut.io.out(7).peek().litValue.intValue & 0xff
      println(s"peeked last decrypted byte = 0x${lastByte.toHexString}")

      val reportedOutLen = dut.io.outLen.peek().litValue.intValue
      println(s"reported outLen = $reportedOutLen")

      val decOutLen = dut.io.outLen.peek().litValue.intValue
      println(s"Decrypt done. outLen = $decOutLen bytes")

      // For "abc", after removing PKCS#7 padding we should get length 3
      assert(decOutLen == 3, s"expected decrypted length 3, got $decOutLen")

      // Print decrypted plaintext bytes
      val decBytes = (0 until decOutLen).map(i => (dut.io.out(i).peek().litValue.intValue & 0xff)).toArray
      println("Decrypted plaintext (ascii): " + decBytes.map(_.toChar).mkString)
    }
  }
}

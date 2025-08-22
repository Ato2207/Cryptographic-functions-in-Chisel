package hashes.sha1

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SHA1Spec extends AnyFlatSpec with ChiselScalatestTester {
  def runSha1Test(msg: String, expectedHex: String, msgBytes: Int): Unit = {
    test(new SHA1Top(msgBytes)) { c =>
      val bytes = msg.getBytes("UTF-8")
      for (i <- bytes.indices) {
        c.io.msg(i).poke(bytes(i).U)
      }

      c.io.msgLen.poke(bytes.length.U)

      // Start signal
      c.io.start.poke(false.B)
      c.clock.step(1)
      c.io.start.poke(true.B)
      c.clock.step(1)
      c.io.start.poke(false.B)

      // Wait for done
      while (!c.io.done.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.io.done.expect(true.B)

      // Build digest bytes from the 5 words
      val digestBytes = c.io.digest.flatMap { word =>
        val w = word.peek().litValue
        Seq(
          ((w >> 24) & 0xff).toByte,
          ((w >> 16) & 0xff).toByte,
          ((w >> 8)  & 0xff).toByte,
          ( w        & 0xff).toByte
        )
      }

      // Convert to hex string
      val gotHex = digestBytes.map("%02x".format(_)).mkString
      assert(gotHex == expectedHex.toLowerCase,
        s"SHA1('$msg') failed: expected $expectedHex but got $gotHex")
    }
  }

  it should "match SHA1(\"\")" in {
    runSha1Test("", "DA39A3EE5E6B4B0D3255BFEF95601890AFD80709", 64)
  }

  it should "match SHA1(\"abc\")" in {
    runSha1Test("abc", "A9993E364706816ABA3E25717850C26C9CD0D89D", 64)
  }

  it should "match SHA1(\"a\" * 100)  (2 blocks, chaining)" in {
    runSha1Test("a" * 100, "7F9000257A4918D7072655EA468540CDCBD42E0C", 128)
  }

  it should "match SHA1((\"abcde\" * 40))  (200 bytes)" in {
    runSha1Test("abcde" * 40, "BFD0F327F747C163337D658B98E0E2C229FC9B17", 256)
  }
}

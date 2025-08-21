package hashes.md5

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class MD5Spec extends AnyFlatSpec with ChiselScalatestTester {
  def runMd5Test(msg: String, expectedHex: String, msgBytes: Int): Unit = {
    test(new MD5Top(msgBytes)) { c =>
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

      // Concatenate digest words into one UInt(128.W)
      val digestBytes = c.io.digest.flatMap { word =>
        val w = word.peek().litValue
        Seq(
          (w & 0xff).toByte,
          ((w >> 8) & 0xff).toByte,
          ((w >> 16) & 0xff).toByte,
          ((w >> 24) & 0xff).toByte
        )
      }

      // Convert to hex string
      val gotHex = digestBytes.map("%02x".format(_)).mkString
      assert(gotHex == expectedHex.toLowerCase,
        f"MD5('$msg') failed: expected $expectedHex but got $gotHex")
    }
  }

  it should "produce the correct MD5 digest for the empty string" in {
    runMd5Test("", "d41d8cd98f00b204e9800998ecf8427e", 64)
  }

  it should "produce the correct MD5 digest for the string 'a'" in {
    runMd5Test("a", "0cc175b9c0f1b6a831c399e269772661", 64)
  }

  it should "produce the correct MD5 digest for the string 'abc'" in {
    runMd5Test("abc", "900150983cd24fb0d6963f7d28e17f72", 64)
  }

  it should "produce the correct MD5 digest for the string 'message digest'" in {
    runMd5Test("message digest", "f96b697d7cb7938d525a2f31aaf161d0", 64)
  }

  it should "produce the correct MD5 digest for the string 'abcdefghijklmnopqrstuvwxyz'" in {
    runMd5Test("abcdefghijklmnopqrstuvwxyz", "c3fcd3d76192e4007dfb496cca67e13b", 128)
  }

  it should "produce the correct MD5 digest for a string which needs block chaining" in {
    runMd5Test("a" * 60, "cc7ed669cf88f201c3297c6a91e1d18d", 128)
  }

  it should "produce the correct MD5 digest for a string with 150 chars" in {
    runMd5Test("a" * 150, "bc620e8c15265f195c8818e2f3e3c58b", 256)
  }
}

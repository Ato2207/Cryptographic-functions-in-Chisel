package hashes.md5

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class MD5Spec extends AnyFlatSpec with ChiselScalatestTester {
  it should "produce the correct MD5 digest for the empty string" in {
    test(new MD5Top(64)) { c =>
      // Prepare inputs: zero-length message -> msgLen = 0
      for (i <- 0 until 64) c.io.msg(i).poke(0.U)
      c.io.msgLen.poke(0.U)

      // ensure start is low, advance 1 cycle
      c.io.start.poke(false.B)
      c.clock.step(1)

      // pulse start for one cycle (rising edge)
      c.io.start.poke(true.B)
      c.clock.step(1)
      c.io.start.poke(false.B)

      // wait for done
      while (!c.io.done.peek().litToBoolean) {
        c.clock.step(1)
      }

      // Now check done and digest
      c.io.done.expect(true.B)

      // Expected MD5("") = d41d8cd98f00b204e9800998ecf8427e
      // A = 0xd98c1dd4, B = 0x04b2008f, C = 0x980980e9, D = 0x7e42f8ec
      c.io.digest(0).expect(BigInt("d98c1dd4", 16))
      c.io.digest(1).expect(BigInt("04b2008f", 16))
      c.io.digest(2).expect(BigInt("980980e9", 16))
      c.io.digest(3).expect(BigInt("7e42f8ec", 16))
    }
  }

  it should "produce the correct MD5 digest for the string 'a'" in {
    test(new MD5Top(64)) { c =>
      // Prepare inputs: message "a" -> ASCII value 0x61
      c.io.msg(0).poke(0x61.U) // ASCII for 'a'
      for (i <- 1 until 64) c.io.msg(i).poke(0.U) // Remaining bytes are zero
      c.io.msgLen.poke(1.U) // Length of the message is 1 byte

      // Ensure start is low, advance 1 cycle
      c.io.start.poke(false.B)
      c.clock.step(1)

      // Pulse start for one cycle (rising edge)
      c.io.start.poke(true.B)
      c.clock.step(1)
      c.io.start.poke(false.B)

      // Wait for done
      while (!c.io.done.peek().litToBoolean) {
        c.clock.step(1)
      }

      // Now check done and digest
      c.io.done.expect(true.B)

      // Expected MD5("a") = 0cc175b9c0f1b6a831c399e269772661
      // A = 0xb975c10c, B = 0xa8b6f1c0, C = 0xe299c331, D = 0x61267769
      c.io.digest(0).expect(BigInt("b975c10c", 16))
      c.io.digest(1).expect(BigInt("a8b6f1c0", 16))
      c.io.digest(2).expect(BigInt("e299c331", 16))
      c.io.digest(3).expect(BigInt("61267769", 16))
    }
  }

  it should "produce the correct MD5 digest for the string 'abc'" in {
    test(new MD5Top(64)) { c =>
      // Prepare inputs: message "abc" -> ASCII values 0x61, 0x62, 0x63
      c.io.msg(0).poke(0x61.U) // ASCII for 'a'
      c.io.msg(1).poke(0x62.U) // ASCII for 'b'
      c.io.msg(2).poke(0x63.U) // ASCII for 'c'
      for (i <- 3 until 64) c.io.msg(i).poke(0.U) // Remaining bytes are zero
      c.io.msgLen.poke(3.U) // Length of the message is 3 bytes

      // Ensure start is low, advance 1 cycle
      c.io.start.poke(false.B)
      c.clock.step(1)

      // Pulse start for one cycle (rising edge)
      c.io.start.poke(true.B)
      c.clock.step(1)
      c.io.start.poke(false.B)

      // Wait for done
      while (!c.io.done.peek().litToBoolean) {
        c.clock.step(1)
      }

      // Now check done and digest
      c.io.done.expect(true.B)

      // Expected MD5("abc") = 900150983cd24fb0d6963f7d28e17f72
      // A = 0x98500190, B = 0xb04fd23c, C = 0x7d3f96d6, D = 0x72f7e128
      c.io.digest(0).expect(BigInt("98500190", 16))
      c.io.digest(1).expect(BigInt("b04fd23c", 16))
      c.io.digest(2).expect(BigInt("7d3f96d6", 16))
      c.io.digest(3).expect(BigInt("727fe128", 16))
    }
  }

  it should "produce the correct MD5 digest for the string 'message digest'" in {
    test(new MD5Top(64)) { c =>
      // Prepare inputs: message "message digest" -> ASCII values
      val message = "message digest".getBytes("UTF-8")
      for (i <- message.indices) {
        c.io.msg(i).poke(message(i).U)
      }
      for (i <- message.length until 64) {
        c.io.msg(i).poke(0.U) // Remaining bytes are zero
      }
      c.io.msgLen.poke(message.length.U) // Length of the message

      // Ensure start is low, advance 1 cycle
      c.io.start.poke(false.B)
      c.clock.step(1)

      // Pulse start for one cycle (rising edge)
      c.io.start.poke(true.B)
      c.clock.step(1)
      c.io.start.poke(false.B)

      // Wait for done
      while (!c.io.done.peek().litToBoolean) {
        c.clock.step(1)
      }

      // Now check done and digest
      c.io.done.expect(true.B)

      // Expected MD5("message digest") = f96b697d7cb7938d525a2f31aaf161d0
      // A = 0x7d696bf9, B = 0x8d93b77c, C = 0x312f5a52, D = 0xd061f1aa
      c.io.digest(0).expect(BigInt("7d696bf9", 16))
      c.io.digest(1).expect(BigInt("8d93b77c", 16))
      c.io.digest(2).expect(BigInt("312f5a52", 16))
      c.io.digest(3).expect(BigInt("d061f1aa", 16))
    }
  }

  it should "produce the correct MD5 digest for the string 'abcdefghijklmnopqrstuvwxyz'" in {
    test(new MD5Top(64)) { c =>
      // Prepare inputs: message "abcdefghijklmnopqrstuvwxyz" -> ASCII values
      val message = "abcdefghijklmnopqrstuvwxyz".getBytes("UTF-8")
      for (i <- message.indices) {
        c.io.msg(i).poke(message(i).U)
      }
      for (i <- message.length until 64) {
        c.io.msg(i).poke(0.U) // Remaining bytes are zero
      }
      c.io.msgLen.poke(message.length.U) // Length of the message

      // Ensure start is low, advance 1 cycle
      c.io.start.poke(false.B)
      c.clock.step(1)

      // Pulse start for one cycle (rising edge)
      c.io.start.poke(true.B)
      c.clock.step(1)
      c.io.start.poke(false.B)

      // Wait for done
      while (!c.io.done.peek().litToBoolean) {
        c.clock.step(1)
      }

      // Now check done and digest
      c.io.done.expect(true.B)

      // Expected MD5("abcdefghijklmnopqrstuvwxyz") = c3fcd3d76192e4007dfb496cca67e13b
      // A = 0xd7c3fcd3, B = 0x00e49261, C = 0xcc49fb7d, D = 0x3be167ca
      c.io.digest(0).expect(BigInt("d7d3fcc3", 16))
      c.io.digest(1).expect(BigInt("00e49261", 16))
      c.io.digest(2).expect(BigInt("6c49fb7d", 16))
      c.io.digest(3).expect(BigInt("3be167ca", 16))
    }
  }
}

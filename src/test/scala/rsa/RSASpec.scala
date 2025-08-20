package rsa

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RSASpec extends AnyFlatSpec with ChiselScalatestTester {
  "RSAKeyGen" should "produce mathematically valid keys for any bit sizes" in {
    val bitsList = Seq(16, 32, 64, 128, 256, 512, 1024, 2048)

    for (bits <- bitsList) {
      test(new RSAKeyGen(keyBits = bits)) { dut =>
        val seeds = Seq(
          0x12345678, // 305419896
          0x0ABCDEF0, // 180150000
          0x1F2E3D4C, // 523310732
          0x0FEDCBA0, // 267242080
        )

        for (seedInt <- seeds) {
          dut.io.seed.poke(seedInt.U(32.W))
          dut.io.start.poke(false.B)
          dut.clock.step(1)
          dut.io.start.poke(true.B)
          dut.clock.step(1)
          dut.io.start.poke(false.B)

          // wait for done
          while (!dut.io.done.peek().litToBoolean) dut.clock.step(1)

          // read outputs
          val nOut = dut.io.n.peek().litValue
          val eOut = dut.io.e.peek().litValue
          val dOut = dut.io.d.peek().litValue

          // sanity checks
          assert(nOut > 0, s"n should be positive, got $nOut")
          assert(eOut > 1, s"e should be > 1, got $eOut")
          assert(dOut > 0, s"d should be positive, got $dOut")

          // RSA property: (e * d) mod phi == 1
          // For testing, recompute phi from n and d/e using Fermat's little theorem workaround:
          // phi is unknown exactly (p and q not visible), but we can check that e*d ≡ 1 mod φ
          // We can check correctness indirectly using (m^e)^d mod n == m for small m
          val testMessage = BigInt(42)
          val enc = testMessage.modPow(eOut, nOut)
          val dec = enc.modPow(dOut, nOut)
          assert(dec == testMessage, s"RSA encryption/decryption failed: got $dec expected $testMessage")
        }
      }
    }
  }

  "RSACore" should "perform modular exponentiation correctly" in {
    test(new RSACore(width = 16)) { dut =>
      // Helper: ensure core is in Idle before starting a new case
      def waitUntilIdle(): Unit = {
        // Hold start low and step until done is false
        dut.io.start.poke(false.B)
        // If we are still in sDone from a previous run, done is true. Drain it.
        while (dut.io.done.peek().litToBoolean) {
          dut.clock.step(1)
        }
        // One extra safety cycle in idle
        dut.clock.step(1)
      }

      // Helper: run one (base, exp, mod) and return result
      def runCase(base: Int, exp: Int, mod: Int): BigInt = {
        waitUntilIdle()

        // Poke inputs
        dut.io.base.poke(base.U)
        dut.io.exp.poke(exp.U)
        dut.io.mod.poke(mod.U)

        // Start pulse
        dut.io.start.poke(true.B)
        dut.clock.step(1)
        dut.io.start.poke(false.B)

        // Wait for completion
        while (!dut.io.done.peek().litToBoolean) {
          dut.clock.step(1)
        }

        dut.io.result.peek().litValue
      }

      val cases = Seq(
        (5, 3, 13, 8),  // 5^3 mod 13 = 125 mod 13 = 8
        (2, 5, 13, 6),  // 2^5 mod 13 = 32 mod 13 = 6
        (7, 2, 10, 9),  // 7^2 mod 10 = 49 mod 10 = 9
        (10, 0, 17, 1), // x^0 mod m = 1
        (3, 7, 11, 9)   // 3^7 mod 11 = 2187 mod 11 = 9
      )

      for ((base, exp, mod, expected) <- cases) {
        val got = runCase(base, exp, mod)
        assert(got == expected,
          s"Case ($base^$exp mod $mod): expected $expected, got $got")
      }
    }
  }

  "RSATop" should "generate keys, encrypt and decrypt correctly for text messages" in {
    val keySizes = Seq(1024, 2048)
    val seeds    = Seq(0x12345, 0xABCDE, 0xFEDCBA)
    val messages = Seq(
      "Hello World",
      "Chisel3 RSA module",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras at mollis turpis, " +
        "et ultrices risus. Morbi euismod mauris nec lorem iaculis, fringilla pellentesque lorem " +
        "porta. Phasellus maximus lectus eget ante pharetra ornare.",
    )

    for (bits <- keySizes) {
      for (seed <- seeds) {
        for (msgStr <- messages) {
          test(new RSATop(keyBits = bits)) { dut =>
            dut.clock.setTimeout(0)

            // Convert message string to BigInt
            val msgBytes = msgStr.getBytes("UTF-8")
            val msgInt   = BigInt(1, msgBytes) // treat as positive number

            // Key Generation
            dut.io.startKeyGen.poke(true.B)
            dut.io.seed.poke(seed.U)
            dut.clock.step(1)
            dut.io.startKeyGen.poke(false.B)

            // Wait for key generation to complete
            while (!dut.io.doneKeyGen.peek().litToBoolean) dut.clock.step(1)

            val n = dut.io.n.peek().litValue
            val e = dut.io.e.peek().litValue
            val d = dut.io.d.peek().litValue
            println(s"Key size $bits, seed $seed: Generated RSA keys: n=$n, e=$e, d=$d")

            // Encrypt
            require(msgInt < n, s"Message too large for modulus $bits-bit RSA. Split into chunks.")
            dut.io.messageIn.poke(msgInt.U)
            dut.io.startEncrypt.poke(true.B)
            dut.clock.step(1)
            dut.io.startEncrypt.poke(false.B)

            // Wait for encryption to complete
            while (!dut.io.doneEncrypt.peek().litToBoolean) dut.clock.step(1)

            // Read ciphertext
            val ciphertext = dut.io.cipherOut.peek().litValue
            println(s"Message '$msgStr' encrypted -> $ciphertext")

            // Decrypt
            dut.io.cipherIn.poke(ciphertext.U)
            dut.io.startDecrypt.poke(true.B)
            dut.clock.step(1)
            dut.io.startDecrypt.poke(false.B)

            // Wait for decryption to complete
            while (!dut.io.doneDecrypt.peek().litToBoolean) dut.clock.step(1)

            // Read decrypted integer
            val decryptedInt = dut.io.messageOut.peek().litValue

            // Convert back to string
            val decryptedBytes = decryptedInt.toByteArray
            val decryptedStr   = new String(decryptedBytes)
            println(s"Ciphertext $ciphertext decrypted -> '$decryptedStr'")

            // Check correctness
            assert(decryptedStr == msgStr, s"Key size $bits, seed $seed: expected '$msgStr', got '$decryptedStr'")
          }
        }
      }
    }
  }
}

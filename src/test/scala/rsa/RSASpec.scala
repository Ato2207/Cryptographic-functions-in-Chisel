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
}

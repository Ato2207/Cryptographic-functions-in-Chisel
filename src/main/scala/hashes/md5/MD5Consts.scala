package hashes.md5

import scala.math.{abs, pow, sin}
import chisel3._

// ---------- Constants ----------
object MD5Consts {
  // K[i] = floor(2^32 * abs(sin(i+1)))
  val K: Seq[UInt] = (0 until 64).map { i =>
    val value = (abs(sin(i + 1)) * pow(2, 32)).toLong
    value.U(32.W)
  }

  // shift amounts S
  val S: Seq[Int] = Seq(
    7,12,17,22, 7,12,17,22, 7,12,17,22, 7,12,17,22,
    5,9,14,20, 5,9,14,20, 5,9,14,20, 5,9,14,20,
    4,11,16,23, 4,11,16,23, 4,11,16,23, 4,11,16,23,
    6,10,15,21, 6,10,15,21, 6,10,15,21, 6,10,15,21
  )

  // --- Helpers ---
  def rol(x: UInt, s: UInt): UInt = (x << s).asUInt | (x >> (32.U - s)).asUInt

  def roundFunc(i: UInt, B: UInt, C: UInt, D: UInt): UInt = {
    Mux(i < 16.U, (B & C) | ((~B).asUInt & D),
      Mux(i < 32.U, (D & B) | ((~D).asUInt & C),
        Mux(i < 48.U, B ^ C ^ D,
          C ^ (B | (~D).asUInt)
        )
      )
    )
  }

  def msgIndex(i: UInt): UInt = {
    Mux(i < 16.U, i,
      Mux(i < 32.U, (5.U * i + 1.U) % 16.U,
        Mux(i < 48.U, (3.U * i + 5.U) % 16.U,
          (7.U * i) % 16.U
        )
      )
    )
  }
}

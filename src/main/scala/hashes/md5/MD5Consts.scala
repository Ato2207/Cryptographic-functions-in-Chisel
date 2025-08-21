package hashes.md5

import chisel3._

// ---------- Constants ----------
object MD5Consts {
  // K[i] = floor(2^32 * abs(sin(i+1)))
  val K: Seq[UInt] = Seq(
    "h_d76aa478".U(32.W), "h_e8c7b756".U, "h_242070db".U, "h_c1bdceee".U,
    "h_f57c0faf".U, "h_4787c62a".U, "h_a8304613".U, "h_fd469501".U,
    "h_698098d8".U, "h_8b44f7af".U, "h_ffff5bb1".U, "h_895cd7be".U,
    "h_6b901122".U, "h_fd987193".U, "h_a679438e".U, "h_49b40821".U,
    "h_f61e2562".U, "h_c040b340".U, "h_265e5a51".U, "h_e9b6c7aa".U,
    "h_d62f105d".U, "h_02441453".U, "h_d8a1e681".U, "h_e7d3fbc8".U,
    "h_21e1cde6".U, "h_c33707d6".U, "h_f4d50d87".U, "h_455a14ed".U,
    "h_a9e3e905".U, "h_fcefa3f8".U, "h_676f02d9".U, "h_8d2a4c8a".U,
    "h_fffa3942".U, "h_8771f681".U, "h_6d9d6122".U, "h_fde5380c".U,
    "h_a4beea44".U, "h_4bdecfa9".U, "h_f6bb4b60".U, "h_bebfbc70".U,
    "h_289b7ec6".U, "h_eaa127fa".U, "h_d4ef3085".U, "h_04881d05".U,
    "h_d9d4d039".U, "h_e6db99e5".U, "h_1fa27cf8".U, "h_c4ac5665".U,
    "h_f4292244".U, "h_432aff97".U, "h_ab9423a7".U, "h_fc93a039".U,
    "h_655b59c3".U, "h_8f0ccc92".U, "h_ffeff47d".U, "h_85845dd1".U,
    "h_6fa87e4f".U, "h_fe2ce6e0".U, "h_a3014314".U, "h_4e0811a1".U,
    "h_f7537e82".U, "h_bd3af235".U, "h_2ad7d2bb".U, "h_eb86d391".U
  )

  // shift amounts S
  val S: Seq[Int] = Seq(
    7,12,17,22, 7,12,17,22, 7,12,17,22, 7,12,17,22,
    5,9,14,20, 5,9,14,20, 5,9,14,20, 5,9,14,20,
    4,11,16,23, 4,11,16,23, 4,11,16,23, 4,11,16,23,
    6,10,15,21, 6,10,15,21, 6,10,15,21, 6,10,15,21
  )

  // --- Helpers ---
  def rol32(v: UInt, s: UInt): UInt = {
    val left = (v << s)(31, 0)
    val right = (v >> (32.U - s))(31, 0)
    left | right
  }

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

package hashes.sha1

import chisel3._

object SHA1Consts {
  // Round constants
  val K0 = "h5A827999".U(32.W) // t =  0..19
  val K1 = "h6ED9EBA1".U(32.W) // t = 20..39
  val K2 = "h8F1BBCDC".U(32.W) // t = 40..59
  val K3 = "hCA62C1D6".U(32.W) // t = 60..79
}
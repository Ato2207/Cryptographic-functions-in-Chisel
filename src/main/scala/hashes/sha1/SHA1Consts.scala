package hashes.sha1

import chisel3._

object SHA1Consts {
  // Round constants
  val K0: UInt = "h5A827999".U(32.W) // t =  0..19
  val K1: UInt = "h6ED9EBA1".U(32.W) // t = 20..39
  val K2: UInt = "h8F1BBCDC".U(32.W) // t = 40..59
  val K3: UInt = "hCA62C1D6".U(32.W) // t = 60..79

  // --- Helper ---
  def rol(x: UInt, s: Int): UInt = (x << s.U).asUInt | (x >> (32.U - s.U)).asUInt
}
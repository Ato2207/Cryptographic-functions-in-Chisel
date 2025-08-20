package rsa

import chisel3._

/**
 * RSA top-level module:
 *  - Runs RSAKeyGen to generate (n, e, d)
 *  - Provides encryption (m^e mod n)
 *  - Provides decryption (c^d mod n)
 */
class RSATop(val keyBits: Int) extends Module {
  val modulusBits: Int = 2 * keyBits

  val io = IO(new Bundle {
    // Control
    val startKeyGen: Bool = Input(Bool())   // Start key generation
    val startEncrypt: Bool = Input(Bool())   // Start encryption
    val startDecrypt: Bool = Input(Bool())   // Start decryption
    val seed: UInt = Input(UInt(32.W))

    // Data
    val messageIn: UInt = Input(UInt(modulusBits.W)) // plaintext
    val cipherIn: UInt = Input(UInt(modulusBits.W)) // ciphertext
    val messageOut: UInt = Output(UInt(modulusBits.W))// decrypted
    val cipherOut: UInt = Output(UInt(modulusBits.W))// encrypted

    // Status + keys
    val doneKeyGen: Bool = Output(Bool())
    val doneEncrypt: Bool = Output(Bool())
    val doneDecrypt: Bool = Output(Bool())
    val n: UInt = Output(UInt(modulusBits.W))
    val e: UInt = Output(UInt(32.W))
    val d: UInt = Output(UInt(modulusBits.W))
  })

  // ----------------------------
  // Key generation submodule
  // ----------------------------
  private val keyGen: RSAKeyGen = Module(new RSAKeyGen(keyBits))
  keyGen.io.start := io.startKeyGen
  keyGen.io.seed  := io.seed

  io.doneKeyGen := keyGen.io.done
  io.n := keyGen.io.n
  io.e := keyGen.io.e
  io.d := keyGen.io.d

  // ----------------------------
  // Encryption / Decryption Core
  // ----------------------------
  private val rsaCore: RSACore = Module(new RSACore(modulusBits))

  // Registers to hold start/done signals for encryption
  private val startEncryptReg: Bool = RegInit(false.B)
  private val doneEncryptReg: Bool = RegInit(false.B)

  private val startDecryptReg: Bool = RegInit(false.B)
  private val doneDecryptReg: Bool = RegInit(false.B)

  // Encryption control
  when(io.startEncrypt) {
    startEncryptReg := true.B
    doneEncryptReg  := false.B
  }

  rsaCore.io.start := startEncryptReg || startDecryptReg
  rsaCore.io.mod   := keyGen.io.n
  rsaCore.io.base  := Mux(startEncryptReg, io.messageIn, io.cipherIn)
  rsaCore.io.exp   := Mux(startEncryptReg, keyGen.io.e, keyGen.io.d)

  when(rsaCore.io.done) {
    when(startEncryptReg) {
      doneEncryptReg  := true.B
      startEncryptReg := false.B
    }
    when(startDecryptReg) {
      doneDecryptReg  := true.B
      startDecryptReg := false.B
    }
  }

  // Decryption control
  when(io.startDecrypt) {
    startDecryptReg := true.B
    doneDecryptReg  := false.B
  }

  // Outputs
  io.doneEncrypt  := doneEncryptReg
  io.doneDecrypt  := doneDecryptReg
  io.cipherOut    := rsaCore.io.result
  io.messageOut   := rsaCore.io.result
}
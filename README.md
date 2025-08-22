# Cryptographic Functions in Chisel

This project provides hardware implementations of several fundamental cryptographic algorithms, written in [Chisel](https://www.chisel-lang.org/).

## Features

- **DES**  

- **RSA**  

- **MD5**  

- **SHA1**

## Testing

All components are verified using [chiseltest](https://www.chisel-lang.org/chiseltest/):

- **DES**:  
  - Tests validate encryption/decryption correctness.
- **RSA**:  
  - Tests check key generation, encryption, and decryption of multiple messages.   
- **MD5**:  
  - Tests compare output digests against standard reference values from RFC 1321.
- **SHA1**:
  - Tests compare output digests against standard reference values from RFC 3174.

Run the tests with:

```bash
sbt test
```

## Project Structure

```
src/
├─ main/scala/
│  ├─ des/           # DESTop, DESCore
│  ├─ rsa/           # RSAKeyGen, RSACore, RSATop
│  └─ hashes/
│     ├─ md5/        # MD5Top, MD5Core, MD5Consts
│     └─ sha1/       # SHA1Top, SHA1Core, SHA1Consts
└─ test/scala/
   ├─ des/           # DESSpec
   ├─ rsa/           # RSASpec
   └─ hashes/
      ├─ md5/        # MD5Spec
      └─ sha1/       # SHA1Spec
```

## Requirements

- [sbt](https://www.scala-sbt.org/)  
- Scala 2.13+  
- Chisel 3.x  

## License

This project is provided under the GNU GPL-3.0 License.

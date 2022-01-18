package gos

import org.junit.Test
import org.junit.Assert.*
import scala.util.Random
import gos.Huffman.*
import gos.Codec.*
import java.nio.charset.StandardCharsets.UTF_8

class HuffmanTest:
  import Tree.*
  @Test def codeTreeCodec =
    val l1 = Leaf('a', 1)
    val l2 = Leaf('b', 42)
    val f = Fork(l1, l2, Vector('a', 'b'), 0)
    val tree = Fork(f, f, Vector('c'), 65)
    assertEquals(tree.encode().decode[Tree](), Right(tree))

  @Test def bitConversion =
    val byte = Random.nextInt.toByte
    assertEquals(toByte(fromByte(byte)), byte)

  @Test def huffmanEncoding =
    val text = Random.nextString(1000)
    val tree = createTree(text)
    assertEquals(decodeHuffman(tree, encodeHuffman(tree, text)), text)

  @Test def losslessCompresion =
    val alphabet = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toVector
    val data =
      Array.fill(1000)(Random.nextInt(alphabet.size)).map(alphabet(_).toByte)
    assertEquals(decompress(compress(data)).toVector, data.toVector)

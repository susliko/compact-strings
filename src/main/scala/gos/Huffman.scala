package gos

import scala.collection.immutable.*
import Codec.*
import java.nio.charset.StandardCharsets.UTF_8

trait Compress {
  def compress(bytes: Array[Byte]): Array[Byte]
  def decompress(bytes: Array[Byte]): Array[Byte]
}

object Huffman extends Compress {
  private val size = 4

  def compress(bytes: Array[Byte]): Array[Byte] =
    val text = new String(bytes, UTF_8)
    val tree = createTree(text)
    val bits = encodeHuffman(tree, text)
    val dataBytes = bits.grouped(8).map(toByte).toArray
    val treeBytes = tree.encode(size)
    treeBytes.size.encodeDelim(size) ++ treeBytes ++
      bits.size.encodeDelim(size) ++
      dataBytes.size.encodeDelim(size) ++ dataBytes

  def decompress(bytes: Array[Byte]): Array[Byte] =
    (for {
      treeSize <- bytes.slice(0, size).decode[Int](size)
      x = size + treeSize
      tree <- bytes.slice(size, x).decode[Tree](size)
      u = x + size
      totalBits <- bytes.slice(x, u).decode[Int](size)
      y = u + size
      dataSize <- bytes.slice(u, y).decode[Int](size)
      compressedData = bytes.slice(y, y + dataSize)
      rawBits = compressedData.toVector.flatMap(fromByte)
      bits = rawBits.take(totalBits)
      data = decodeHuffman(tree, bits).getBytes(UTF_8)
    } yield data).toOption.get

  enum Tree:
    case Fork(
        left: Tree,
        right: Tree,
        chars: Vector[Char],
        weight: Int
    )
    case Leaf(char: Char, weight: Int)

  object Tree:
    given Codec[Tree] with
      def encode(t: Tree, db: Int): Array[Byte] =
        t match
          case f: Fork =>
            val tpe = Array('F'.toByte)
            val left = encode(f.left, db)
            val right = encode(f.right, db)
            val chars = f.chars.toArray.map(_.toByte)
            val weight = f.weight.encode(size)
            tpe ++ left.size.encodeDelim(size) ++ left ++
              right.size.encodeDelim(size) ++ right ++
              chars.size.encodeDelim(size) ++ chars ++
              weight

          case l: Leaf =>
            val tpe = Array('L'.toByte)
            val char = Array(l.char.toByte)
            val weight = l.weight.encode(size)
            tpe ++ char ++ weight

      def decode(
          bytes: Array[Byte],
          db: Int
      ): Either[Codec.Failure, Tree] =
        val tpe = bytes(0).toChar
        tpe match {
          case 'F' =>
            for {
              leftSize <- bytes.slice(1, 5).decode[Int](size)
              x = 5 + leftSize
              left <- bytes.slice(5, x).decode[Tree](db)
              y = x + 4
              rightSize <- bytes.slice(x, y).decode[Int](size)
              z = y + rightSize
              right <- bytes.slice(y, z).decode[Tree](db)
              w = z + 4
              charsSize <- bytes.slice(z, w).decode[Int](size)
              u = w + charsSize
              chars = bytes.slice(w, u).map(_.toChar).toVector
              weight <- bytes.slice(u, u + 4).decode[Int](size)
            } yield Fork(left, right, chars, weight)
          case 'L' =>
            val char = bytes(1).toChar
            bytes.slice(2, 6).decode[Int](size).map(Leaf(char, _))

          case c => Left(Codec.Failure(s"Unknown char $c"))
        }

  import Tree.*

  def toByte(bits: Vector[Int]): Byte =
    bits.zipWithIndex.foldLeft[Byte](0) { case (acc, (bit, i)) =>
      (acc + (bit << (7 - i))).toByte
    }

  def fromByte(b: Byte): Vector[Int] =
    (0 until 8).foldLeft(Vector.empty[Int]) { case (acc, i) =>
      ((b >> i) & 1) +: acc
    }

  def encodeHuffman(tree: Tree, text: String): Vector[Int] =
    val table = treeToTable(tree)
    text.toVector
      .flatMap(char => table.filter((code) => code._1 == char).head._2)

  def decodeHuffman(tree: Tree, bits: Vector[Int]): String =
    def traverse(remaining: Tree, bits: Vector[Int]): Vector[Char] =
      remaining match {
        case Leaf(c, _) if bits.isEmpty => Vector(c)
        case Leaf(c, _)                 => c +: traverse(tree, bits)
        case Fork(left, right, _, _) if bits.head == 0 =>
          traverse(left, bits.tail)
        case Fork(left, right, _, _) => traverse(right, bits.tail)
      }

    traverse(tree, bits).mkString

  private def weight(tree: Tree): Int = tree match
    case Fork(_, _, _, w) => w
    case Leaf(_, w)       => w

  private def chars(tree: Tree): Vector[Char] = tree match
    case Fork(_, _, cs, _) => cs
    case Leaf(c, _)        => Vector(c)

  private def times(chars: Vector[Char]): Vector[(Char, Int)] =
    def incr(acc: Map[Char, Int], c: Char) =
      val count = (acc get c).getOrElse(0) + 1
      acc + ((c, count))
    chars.foldLeft(Map.empty[Char, Int])(incr).iterator.toVector

  private def combineTrees(trees: Vector[Tree]): Vector[Tree] = trees match
    case left +: right +: cs =>
      val ch = chars(left) ++ chars(right)
      val w = weight(left) + weight(right)
      (Fork(left, right, ch, w) +: cs).sortWith((t1, t2) =>
        weight(t1) < weight(t2)
      )
    case _ => trees

  def createTree(text: String): Tree =
    def go(p: Vector[Tree] => Boolean, f: Vector[Tree] => Vector[Tree])(
        trees: Vector[Tree]
    ): Vector[Tree] = if (p(trees)) trees else go(p, f)(f(trees))
    go(_.size == 1, combineTrees)(
      times(text.toVector)
        .sortWith((f1, f2) => f1._2 < f2._2)
        .map((f) => Leaf(f._1, f._2))
    ).head

  private type Code = (Char, Vector[Int])
  private type Table = Vector[Code]

  private def treeToTable(tree: Tree): Table = tree match
    case Leaf(c, w) => Vector((c, Vector()))
    case Fork(left, right, cs, w) =>
      mergeTables(treeToTable(left), treeToTable(right))

  private def mergeTables(a: Table, b: Table): Table =
    def prepend(b: Int)(code: Code): Code = (code._1, b +: code._2)
    a.map(prepend(0)) ++ b.map(prepend(1))
}

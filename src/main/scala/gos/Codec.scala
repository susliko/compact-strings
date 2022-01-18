package gos

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scala.util.Try
import java.nio.charset.StandardCharsets.UTF_8

trait Codec[T]:
  def encode(t: T, delimBytes: Int): Array[Byte]
  def decode(bytes: Array[Byte], delimBytes: Int): Either[Codec.Failure, T]

object Codec:
  case class Failure(reason: String)

  // Codec syntax
  extension [T](t: T)
    def encode(delimBytes: Int = 2)(using codec: Codec[T]): Array[Byte] =
      codec.encode(t, delimBytes)

  extension (i: Int)
    def encodeDelim(delimBytes: Int): Array[Byte] =
      i.encode(delimBytes).drop(4 - delimBytes)

  extension (bytes: Array[Byte])
    def decode[T](delimBytes: Int = 2)(using
        codec: Codec[T]
    ): Either[Codec.Failure, T] =
      codec.decode(bytes, delimBytes)

  extension (bytes: Array[Byte])
    def decodeDelim(delimBytes: Int): Int =
      bytes.decode[Int](delimBytes).toOption.get

  // Codec instances
  given Codec[Int] with
    def encode(t: Int, _db: Int): Array[Byte] =
      Array(
        (t >> 24).toByte,
        (t << 8 >> 24).toByte,
        (t << 16 >> 24).toByte,
        (t << 24 >> 24).toByte
      )
    def decode(
        bytes: Array[Byte],
        _db: Int
    ): Either[Failure, Int] =
      if (bytes.size > 4)
        Left(Failure(s"Less than 4 bytes expected for Int, got: ${bytes.size}"))
      else
        Right(bytes.zipWithIndex.foldRight(0) { case ((b, i), acc) =>
          acc + ((b & 0xff) << (bytes.size - 1 - i) * 8)
        })

  given Codec[String] with
    def encode(t: String, _db: Int): Array[Byte] = t.getBytes(UTF_8)
    def decode(bytes: Array[Byte], _db: Int): Either[Failure, String] =
      Right(new String(bytes, UTF_8))

  given [T: ClassTag: Codec]: Codec[Vector[T]] with
    def encode(ts: Vector[T], delimBytes: Int): Array[Byte] =
      ts.toArray.flatMap(t =>
        val bytes = t.encode(delimBytes)
        if (bytes.size > 0) bytes.size.encodeDelim(delimBytes) ++ bytes
        else Array.empty[Byte]
      )
    def decode(
        bytes: Array[Byte],
        delimBytes: Int
    ): Either[Failure, Vector[T]] =
      def unsafeDecode =
        var i = 0
        var words: Vector[T] = Vector.empty
        var failure: Failure = null
        while (failure.eq(null) && i < bytes.size) {
          val wordBytes = bytes.slice(i, i + delimBytes).decodeDelim(delimBytes)
          i += delimBytes
          val word = bytes.slice(i, i + wordBytes).decode[T](delimBytes)
          word match
            case e @ Left(f) => failure = f
            case Right(w)    => words = words :+ w
          i += wordBytes
        }
        if (words.isEmpty && failure.eq(null)) Right(Vector.empty)
        else if (!failure.eq(null)) Left(failure)
        else Right(words)
      Try(unsafeDecode).toEither
        .fold(
          e => Left(Failure(s"Failed to decode vector: ${e.toString}")),
          identity
        )

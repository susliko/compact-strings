package gos

import org.junit.Test
import org.junit.Assert.*
import gos.*
import gos.Codec.*
import scala.util.Random

class CodecTest:
  @Test def stringCodec: Unit =
    val string = Random.nextString(1000)
    assertEquals(string.encode().decode[String](), Right(string))

  @Test def intCodec: Unit =
    assertEquals(0.encode().decode[Int](), Right(0))
    assertEquals(Int.MaxValue.encode().decode[Int](), Right(Int.MaxValue))
    assertEquals(Int.MinValue.encode().decode[Int](), Right(Int.MinValue))

  @Test def vectorCodec: Unit =
    val emptyVec = Vector.empty[Int].encode().decode[Vector[Int]]()
    assertEquals(emptyVec, Right(Vector.empty[Int]))
    val randomVec = Vector.fill(1000)(Random.nextInt)
    assertEquals(randomVec.encode().decode[Vector[Int]](), Right(randomVec))

  @Test def variousDelimSizes: Unit =
    val data = Vector(0, 0, 0)
    assertEquals(data.encode(1).size, 12 + 1 * 3)
    assertEquals(data.encode(2).size, 12 + 2 * 3)
    assertEquals(data.encode(3).size, 12 + 3 * 3)
    assertEquals(data.encode(4).size, 12 + 4 * 3)

  @Test def diagnosticsCodec: Unit =
    val data = Diagnostics(
      Vector.fill(100)(Random.nextString(1000)).map(Template.apply),
      Vector.fill(100)(
        Diagnostics.Message(
          Random.nextInt,
          Vector.fill(100)(Random.nextString(100))
        )
      )
    )
    assertEquals(data.encode(4).decode[Diagnostics](4), Right(data))

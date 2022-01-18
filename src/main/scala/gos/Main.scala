package gos
import gos.Codec
import gos.Codec.*
import java.nio.charset.StandardCharsets.UTF_8

case class Diagnostics(
    templates: Vector[Template],
    messages: Vector[Diagnostics.Message]
):
  def compile: Vector[String] = messages.map(m =>
    val template = templates(m.template)
    template.interpolate(m.data: _*)
  )

object Diagnostics:
  case class Message(template: Int, data: Vector[String])
  object Message:
    given Codec[Message] with
      def encode(t: Message, db: Int): Array[Byte] =
        val template = t.template.encode(4)
        val data = t.data.encode(db)
        template ++
          data.size.encodeDelim(db) ++
          data
      def decode(
          bytes: Array[Byte],
          db: Int
      ): Either[Codec.Failure, Message] =
        for {
          template <- bytes.slice(0, 4).decode[Int](4)
          x = 4 + db
          dataSize <- bytes.slice(4, x).decode[Int](db)
          data <- bytes.slice(x, x + dataSize).decode[Vector[String]](db)
        } yield Message(template, data)

  given Codec[Diagnostics] with
    def encode(t: Diagnostics, db: Int): Array[Byte] =
      val templates = t.templates.encode(db)
      val messages = t.messages.encode(db)
      templates.size.encodeDelim(db) ++
        templates ++
        messages.size.encodeDelim(db) ++
        messages
    def decode(
        bytes: Array[Byte],
        db: Int
    ): Either[Codec.Failure, Diagnostics] = for {
      templatesSize <- bytes.slice(0, db).decode[Int](db)
      templates <- bytes
        .slice(db, db + templatesSize)
        .decode[Vector[Template]](db)
      x = db + templatesSize
      messagesSize <- bytes.slice(x, x + db).decode[Int](db)
      y = x + db
      messages <- bytes.slice(y, y + messagesSize).decode[Vector[Message]](db)
    } yield Diagnostics(templates, messages)

@main def example: Unit =
  import Diagnostics.*
  val t1 = Template(
    "К сожалению, сервис {0} недоступен по причине {1}"
  )
  val t2 = Template(
    "Оплата {0} успешна. Заказ доставит курьер {1} по адресу {2}"
  )
  val t1m1 = Message(0, Vector("PURCHASES", "RELEASE"))
  val t1m2 = Message(0, Vector("PAYMENTS", "MAINTANANCE"))
  val t2m1 = Message(1, Vector("0", "Зик Егер", "レベリオ"))
  val t2m2 = Message(1, Vector("1", "Эрен Егер", "パラディ"))
  val diagnostics = Diagnostics(Vector(t1, t2), Vector(t1m1, t1m2, t2m1, t2m2))

  val processedDiagnostics =
    diagnostics
      .encode(delimBytes = 2)
      .decode[Diagnostics](delimBytes = 2)

  processedDiagnostics.fold(
    e => println(e),
    d => println(d.compile.mkString("\n"))
  )

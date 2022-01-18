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
        val template = t.template.encode(db)
        val data = t.data.encode(db)
        template.size.encodeDelim(db) ++
          template ++
          data.size.encodeDelim(db) ++
          data
      def decode(
          bytes: Array[Byte],
          db: Int
      ): Either[Codec.Failure, Message] =
        for {
          templateSize <- bytes.slice(0, db).decode[Int](db)
          template <- bytes.slice(db, db + templateSize).decode[Int](db)
          x = db + templateSize
          dataSize <- bytes.slice(x, x + db).decode[Int](db)
          y = x + db
          data <- bytes.slice(y, y + dataSize).decode[Vector[String]](db)
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
  val t1 = "К сожалению, сервис {0} недоступен по причине {1}"
  val t2 = "Заказ {0} оформлен, его доставит курьер {1} по адресу {2}"
  val t1m1 = Message(0, Vector("ПОКУПОК", "ПАДЕНИЯ"))
  val t1m2 = Message(0, Vector("ПРОДАЖ", "ПОДЪЁМА"))
  val t2m1 = Message(1, Vector("A", "ГАМЛЕТ", "ул. Сезам, д. 42"))
  val t2m2 = Message(1, Vector("B", "ИМАДЖОН", "ул. Ленина, д. 0"))
  val diagnostics = Diagnostics(Vector(t1, t2), Vector(t1m1, t1m2, t2m1, t2m2))

  val serdeDiagnostics =
    diagnostics.encode(delimBytes = 2).decode[Diagnostics](delimBytes = 2)
  println(serdeDiagnostics.toOption.get.compile.mkString("\n"))

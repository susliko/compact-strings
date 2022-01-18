package gos
import java.text.MessageFormat

opaque type Template = String
object Template:
  def apply(s: String): Template = s
  given (using sCodec: Codec[String]): Codec[Template] =
    sCodec.asInstanceOf[Codec[Template]]
  extension (t: Template)
    def interpolate(args: Any*): String = MessageFormat.format(t, args: _*)

package simple_lang_typechecker.unification

trait Term[T] {
  def asString: String
}
case class PlaceholderTerm[T](placeholder: Long) extends Term[T] {
  def asString: String = "_" + placeholder
}
case class IntTerm(around: Int) extends Term[Int] {
  def asString: String = around.toString
}
case class StructureTerm[T](val name: String, val subterms: Seq[Term[_]]) extends Term[T] {
  def asString: String = {
    if (subterms.isEmpty) {
      name
    } else {
      name + "(" + subterms.map(_.asString).mkString(", ") + ")"
    }
  }
}

object VariantOf {
  def apply[T](name: String, params: Term[_]*): StructureTerm[T] = {
    StructureTerm(name, params.toSeq)
  }
}

object IntTerm {
  import scala.language.implicitConversions
  implicit def intToIntTerm(i: Int): IntTerm = IntTerm(i)
}

object NewVariable {
  import java.util.concurrent.atomic.AtomicLong
  private val nextId = new AtomicLong()
  def apply[T](): Term[T] = PlaceholderTerm(nextId.getAndIncrement())
}

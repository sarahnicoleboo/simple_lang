package simple_lang_typechecker.unification

class UnificationEnvironment(private val mapping: Map[Long, Term[_]]) {
  def this() = this(Map())

  @scala.annotation.tailrec
  final def lookup(t: Term[_]): Term[_] = {
    t match {
      case PlaceholderTerm(l) if mapping.contains(l) =>
        lookup(mapping(l))
      case _ => t
    }
  } // lookup

  def fullLookup(t: Term[_]): Term[_] = {
    lookup(t) match {
      case p@PlaceholderTerm(_) => p
      case i@IntTerm(_) => i
      case StructureTerm(name, subterms) => {
        new StructureTerm(name, subterms.map(fullLookup))
      }
    }
  } // fullLookup

  def termContains(t: Term[_], p: PlaceholderTerm[_]): Boolean = {
    t match {
      case IntTerm(_) => false
      case StructureTerm(_, subterms) => subterms.exists(termContains(_, p))
      case otherP@PlaceholderTerm(_) => {
        lookup(otherP) match {
          case `p` => true // same placeholder
          case PlaceholderTerm(_) => false // other placeholder
          case other => termContains(other, p)
        }
      }
    }
  } // termContains

  private def unifyPlaceholderTerm(p: PlaceholderTerm[_], t: Term[_]): Option[UnificationEnvironment] = {
    if (p == t) {
      Some(this)
    } else if (!termContains(t, p)) {
      Some(new UnificationEnvironment(mapping + (p.placeholder -> t)))
    } else {
      None
    }
  } // unifyPlaceholderTerm

  def unify[T](t1: Term[T], t2: Term[T]): Option[UnificationEnvironment] = {
    unifyInternal(t1, t2)
  } // unify

  private def unifyInternal(t1: Term[_], t2: Term[_]): Option[UnificationEnvironment] = {
    (lookup(t1), lookup(t2)) match {
      case (IntTerm(x), IntTerm(y)) if x == y => Some(this)
      case (p@PlaceholderTerm(_), t) => unifyPlaceholderTerm(p, t)
      case (t, p@PlaceholderTerm(_)) => unifyPlaceholderTerm(p, t)
      case (StructureTerm(name1, subterms1), StructureTerm(name2, subterms2)) if name1 == name2 && subterms1.size == subterms2.size => {
        subterms1.zip(subterms2).foldLeft(Some(this): Option[UnificationEnvironment])((res, cur) => {
          res.flatMap(_.unifyInternal(cur._1, cur._2))
        })
      }
      case _ => None
    }
  } // unifyInternal

  def prettyTerm(term: Term[_]): String = {
    fullLookup(term).asString
  } // prettyTerm

  override def toString: String = mapping.toString
} // UnificationEnvironment

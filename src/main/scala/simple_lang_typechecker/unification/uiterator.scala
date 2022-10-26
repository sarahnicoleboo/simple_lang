package simple_lang_typechecker.unification

object UIterator {
  def empty[S]: UIterator[S, Nothing] = {
    new UIterator[S, Nothing] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, Nothing)] = Iterator()
    }
  } // empty

  def emptyType[S, A]: UIterator[S, A] = {
    new UIterator[S, A] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, A)] = Iterator()
    }
  } // empty

  def singleton[S, A](a: A): UIterator[S, A] = {
    new UIterator[S, A] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, A)] = Iterator((env, state, a))
    }
  } // singleton

  def getState[S]: UIterator[S, S] = {
    new UIterator[S, S] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, S)] = Iterator((env, state, state))
    }
  } // getState

  def putState[S](newState: S): UIterator[S, Unit] = {
    new UIterator[S, Unit] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, Unit)] = Iterator((env, newState, ()))
    }
  } // putState

  def modState[S](f: S => S): UIterator[S, Unit] = {
    for {
      state1 <- getState[S]
      _ <- putState(f(state1))
    } yield ()
  } // modState

  def unify[S, T](t1: Term[T], t2: Term[T]): UIterator[S, Unit] = {
    new UIterator[S, Unit] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, Unit)] = {
        env.unify(t1, t2).toIterator.map(newEnv => (newEnv, state, ()))
      }
    }
  }

  def disjuncts[S, A](its: UIterator[S, A]*): UIterator[S, A] = {
    its.reduceRight(_ ++ _)
  } // disjuncts

  def multiUnify[S, A, T](on: Term[T])(cases: (Term[T], () => UIterator[S, A])*): UIterator[S, A] = {
    cases.foldRight(emptyType[S, A])((pair, result) => {
      val (term, doThis) = pair
      unify(on, term).flatMap(_ => doThis()) ++ result
    })
  } // multiUnify

  def rule[S, A, T](term: Term[T])(body: => UIterator[S, A]): (Term[T], () => UIterator[S, A]) = (term, () => body)

  def toUIterator[S, A](it: Iterator[A]): UIterator[S, A] = {
    new UIterator[S, A] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, A)] = {
        it.map(a => (env, state, a))
      }
    }
  } // toUIterator
} // UIterator

// S: State
// A: value that the iterator produces
trait UIterator[S, +A] {
  self =>

  def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, A)]

  def flatMap[B](f: A => UIterator[S, B]): UIterator[S, B] = {
    new UIterator[S, B] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, B)] = {
        self.reify(env, state).flatMap(
          { case (newEnv, newState, a) => f(a).reify(newEnv, newState) })
      }
    }
  } // flatMap

  def map[B](f: A => B): UIterator[S, B] = {
    new UIterator[S, B] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, B)] = {
        self.reify(env, state).map(
          { case (newEnv, newState, b) => (newEnv, newState, f(b)) })
      }
    }
  } // map

  def ++[B >: A](other: UIterator[S, B]): UIterator[S, B] = {
    new UIterator[S, B] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, B)] = {
        self.reify(env, state) ++ other.reify(env, state)
      }
    }
  } // ++

  def withFilter(p: A => Boolean): UIterator[S, A] = {
    new UIterator[S, A] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, A)] = {
        self.reify(env, state).withFilter(
          { case (_, _, a) => p(a) })
      }
    }
  } // withFilter

  // the other is only used if there were no results from me
  def onlyOnFailure[B >: A](other: => UIterator[S, B]): UIterator[S, B] = {
    var succeeded = false
    object OtherWrapper extends UIterator[S, B] {
      def reify(env: UnificationEnvironment, state: S): Iterator[(UnificationEnvironment, S, B)] = {
        if (succeeded) Iterator() else other.reify(env, state)
      }
    }

    self.map(a => {
      succeeded = true
      a
    }) ++ OtherWrapper
  } // onlyOnFailure

  def printOnFailure(msg: String): UIterator[S, A] = {
    onlyOnFailure {
      println(msg)
      UIterator.empty
    }
  } // printOnFailure
} // UIterator

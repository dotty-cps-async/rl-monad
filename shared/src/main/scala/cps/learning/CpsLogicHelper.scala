package cps.learning

import cps.*
import cps.monads.logic.CpsLogicMonad


def fromObserver[F[_], A](using m: CpsLogicMonad[F])(fa: m.Observer[A]): F[A] =
  m.flattenObserver(
    m.observerCpsMonad.map(fa)(a => m.pure(a))
  )



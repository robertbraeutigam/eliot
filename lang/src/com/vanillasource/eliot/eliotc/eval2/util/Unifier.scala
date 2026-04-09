package com.vanillasource.eliot.eliotc.eval2.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval2.fact.*
import com.vanillasource.eliot.eliotc.eval2.util.MetaState.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Pattern unification for Sem values. After forcing both sides, performs structural unification with trivial
  * meta-solving (empty-spine metas only for now).
  */
object Unifier {

  /** Unify two Sem values. Reports a type error via the provided source position if they don't match.
    */
  def unify(s1: Sem, s2: Sem, source: Sourced[?]): EvalIO[Unit] =
    for {
      f1 <- force(s1)
      f2 <- force(s2)
      _  <- unifyForced(f1, f2, source)
    } yield ()

  private def unifyForced(s1: Sem, s2: Sem, source: Sourced[?]): EvalIO[Unit] = (s1, s2) match {
    case (Sem.Lit(d1), Sem.Lit(d2)) =>
      if (d1 == d2) ().pure[EvalIO]
      else typeError(s1, s2, source)

    case (Sem.TypeUniv, Sem.TypeUniv) =>
      ().pure[EvalIO]

    case (Sem.Struct(t1, f1), Sem.Struct(t2, f2)) =>
      if (t1 == t2 && f1.keySet == f2.keySet)
        f1.toList.traverse_ { case (name, v1) => unify(v1, f2(name), source) }
      else typeError(s1, s2, source)

    case (Sem.Lam(_, dom1, c1), Sem.Lam(_, dom2, c2)) =>
      for {
        _        <- unify(dom1, dom2, source)
        fresh     = Sem.Neut(Head.Param(s"$$unify_${System.nanoTime}"))
        applied1 <- Evaluator2.apply(Sem.Lam("_", dom1, c1), fresh)
        applied2 <- Evaluator2.apply(Sem.Lam("_", dom2, c2), fresh)
        _        <- unify(applied1, applied2, source)
      } yield ()

    case (Sem.Neut(h1, sp1), Sem.Neut(h2, sp2)) if h1 == h2 && sp1.length == sp2.length =>
      sp1.zip(sp2).traverse_ { case (a, b) => unify(a, b, source) }

    case (Sem.Neut(Head.Meta(id), spine), other) =>
      solveFlex(id, spine, other, source)

    case (other, Sem.Neut(Head.Meta(id), spine)) =>
      solveFlex(id, spine, other, source)

    case _ =>
      typeError(s1, s2, source)
  }

  /** Solve a flex pattern `?m(spine) = other`.
    *
    * For the common case of empty spine, this directly solves `?m = other`. For non-empty spines of distinct Param
    * neutrals, this performs pattern unification by substituting params in the solution.
    */
  private def solveFlex(id: MetaId, spine: Seq[Sem], other: Sem, source: Sourced[?]): EvalIO[Unit] = {
    if (occursCheck(id, other)) {
      liftCompilerIO(Sourced.compilerAbort(source.as("Infinite type detected during unification.")))
    } else if (spine.isEmpty) {
      solveMeta(id, other)
    } else {
      val params = spine.collect { case Sem.Neut(Head.Param(name), Seq()) => name }
      if (params.length != spine.length || params.distinct.length != params.length) {
        liftCompilerIO(
          Sourced.compilerAbort(
            source.as("Could not infer type."),
            Seq("Non-pattern spine in metavariable application.")
          )
        )
      } else {
        liftCompilerIO(
          Sourced.compilerAbort(
            source.as("Could not infer type."),
            Seq("Higher-order pattern unification not yet supported.")
          )
        )
      }
    }
  }

  /** Check if a metavariable occurs in a Sem value (infinite type check). */
  private def occursCheck(id: MetaId, sem: Sem): Boolean = sem match {
    case Sem.Lit(_)                      => false
    case Sem.TypeUniv                    => false
    case Sem.Struct(_, fields)           => fields.values.exists(occursCheck(id, _))
    case Sem.Lam(_, dom, _)              => occursCheck(id, dom)
    case Sem.Neut(Head.Meta(mid), spine) => mid == id || spine.exists(occursCheck(id, _))
    case Sem.Neut(_, spine)              => spine.exists(occursCheck(id, _))
  }

  private def typeError(s1: Sem, s2: Sem, source: Sourced[?]): EvalIO[Unit] =
    liftCompilerIO(
      Sourced.compilerAbort(
        source.as("Type mismatch."),
        Seq(s"Cannot unify '${showSem(s1)}' with '${showSem(s2)}'.")
      )
    )

  /** Simple display of Sem for error messages. */
  private[eval2] def showSem(sem: Sem): String = sem match {
    case Sem.Lit(d)            => d.value.toString
    case Sem.TypeUniv          => "Type"
    case Sem.Struct(fqn, _)    => fqn.name.name
    case Sem.Lam(p, _, _)      => s"($p -> ...)"
    case Sem.Neut(head, spine) =>
      val headStr = head match {
        case Head.Param(name) => name
        case Head.Meta(id)    => s"?${id.raw}"
        case Head.Ref(vfqn)   => vfqn.name.name
      }
      if (spine.isEmpty) headStr
      else s"$headStr(${spine.map(showSem).mkString(", ")})"
  }
}

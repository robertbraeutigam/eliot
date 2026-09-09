package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.{
  cellReadFQN,
  cellWriteFQN,
  escapeFQN,
  exitFQN,
  leftFQN,
  pairConstructorFQN,
  rightFQN,
  typeFQN,
  withCellFQN
}
import com.vanillasource.eliot.eliotc.monomorphize.domain.{MetaStore, SemValue}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.{ConcreteNormalForm, Evaluator}

import scala.util.DynamicVariable
import scala.util.control.ControlThrowable

/** The compiler platform's two control-flow primitives — **escape** and **cell** — as evaluator intrinsics (effects v6,
  * `docs/effects.md` §9.6, §10.1 step 7). A finishing clause is a non-local exit and a stateful implementation needs a
  * value threaded through calls that never mention it; neither is expressible as a def in a strict pure core, so each
  * platform has them as private leaves — an exception and a static field on the jvm, and here, on the compile track,
  * two natives the one NbE evaluator runs directly. At the flag day the compile-track overlay bodies `runThrow`,
  * `catch`, `else`, `runAbort` and `runState*` over these, and the `Either`-based `AbortCarrier` overlay goes.
  *
  * '''Frames, and the machine stack's discipline.''' `escape` and `withCell` each install a **frame** for the extent
  * of one call and remove it when the call returns or is exited through; `exit`, `read` and `write` reach the
  * **nearest enclosing frame of their instantiation** and nothing else. That is the discipline of the machine stack,
  * kept inside the leaf and nowhere else (§12, "a runtime handler stack"): the frames are a dynamically scoped stack
  * of the evaluator's own, the escape is a Scala control exception caught by exactly the frame it names, and no frame
  * is ever reachable after its call — so nothing can dangle.
  *
  * '''The instantiation is a type *value*, passed as the leading argument.''' The frame an operation reaches is the
  * nearest one of *its instantiation*: `raise(msg)` at `E := String` inside an `else` at `E := Unit` (the ordinary
  * `if(c) T else raise(msg)` guard) must pass through the inner frame to the outer one, exactly as the jvm's
  * one-exception-class-per-instantiation does. The evaluator cannot read an instantiation off type arguments: the
  * post-monomorphization [[com.vanillasource.eliot.eliotc.monomorphize.eval.MonomorphicEvaluator]] erases them, and
  * every binding is looked up by FQN alone, so one native serves every instantiation. Types are values, so the overlay
  * passes the instantiation itself — `escape(E, body)`, `exit(E, err)`, `withCell(S, s0, body)`, `read(S)`,
  * `write(S, s)` — and a frame matches a key by definitional equality of concrete normal forms
  * ([[ConcreteNormalForm]]). A key that is not concrete matches nothing, and a native that finds no frame (or is handed
  * no concrete key) stays **stuck** on its own FQN rather than answering: a use the scope check did not cover is a loud
  * read-back error, never a silently wrong value.
  *
  * '''Strictness at the frame.''' The evaluator applies a top-level definition lazily — the application only grows the
  * spine, and the body runs when the value is forced — so a thunk's *result* may still hold an application that would
  * `exit` or `write` when forced. Every value that crosses a frame boundary is therefore **settled** inside the frame
  * (forced, and its constructor and stuck-native spines with it — [[Evaluator.renormalize]]'s traversal, which stops
  * at a lambda since a lambda's calls run later, at a rowless slot the scope check keeps away from these frames):
  * the thunk's result before the frame is left, an `exit`'s payload before the exit is taken, a cell's initial and
  * written content. Pending applications settle in that traversal's order (head, then arguments left to right), which
  * is the one evaluation order the compile track has.
  *
  * '''What a caller must know.''' A nullary top-level definition's body is evaluated **once** per binding
  * ([[SemValue.Lazy]]), so a nullary def that `read`s a cell would answer its first read forever; a def that reaches a
  * cell must take an argument (a thunk does), or its binding must not be memoised — a concern for the overlay that
  * bodies the `State` implementation, recorded there.
  *
  * '''What the intrinsics answer is the overlay's data.''' `escape` answers `Left`/`Right` and `withCell` answers
  * `Pair`, each the value constructor the compile-track overlay declares (`stdlib/eliot-compiler/eliot/lang/`), applied
  * to exactly its fields — a `match` applies a handler to every spine entry — so the overlay's own `foldEither` and
  * `foldPair` take them apart like any constructed value. A twin eliminator over a private normal form was tried and
  * does not work: the deep escalation links a bodied definition *reduced at its instantiation* ahead of a raw native,
  * so a borrowed `foldPair` body always won and could not read the private form.
  */
object EffectIntrinsics {

  /** The exact-FQN intrinsics, folded into [[SystemNativesProcessor]]'s system reductions. */
  val bindings: Map[ValueFQN, SemValue] = Map(
    escapeFQN    -> escapeNative,
    exitFQN      -> exitNative,
    withCellFQN  -> withCellNative,
    cellReadFQN  -> readNative,
    cellWriteFQN -> writeNative
  )

  /** The declared type of every key parameter: the type `Type`, not [[VType]] itself — a native whose parameter is
    * `VType` models *every* written type argument, and a key must arrive as a value, never be taken from a type
    * argument the reference happens to write (a phantom binder, an explicit `[E]`).
    */
  private val keyType: SemValue = VTopDef(typeFQN, None, Spine.SNil)

  private sealed trait Frame {
    def key: SemValue
  }

  private final class EscapeFrame(val key: SemValue) extends Frame

  private final class CellFrame(val key: SemValue, var content: SemValue) extends Frame

  /** The non-local exit in flight: names the one frame that catches it, so every frame between rethrows by not
    * matching. A control throwable — no stack trace, and not a `NonFatal` that a generic handler would swallow.
    */
  private final case class Exiting(frame: EscapeFrame, value: SemValue) extends ControlThrowable

  /** The active frames, innermost first. Dynamically scoped: a frame is on the stack exactly for the extent of its
    * intrinsic's call, restored on return and on exit alike.
    */
  private val frames: DynamicVariable[List[Frame]] = new DynamicVariable(Nil)

  private def inFrame[A](frame: Frame)(body: => A): A = frames.withValue(frame :: frames.value)(body)

  private def nearestEscape(key: SemValue): Option[EscapeFrame] =
    frames.value.collectFirst { case frame: EscapeFrame if ConcreteNormalForm.equal(frame.key, key) => frame }

  private def nearestCell(key: SemValue): Option[CellFrame] =
    frames.value.collectFirst { case frame: CellFrame if ConcreteNormalForm.equal(frame.key, key) => frame }

  /** Force a value that is about to cross a frame boundary, so every pending application inside it runs while the frame
    * is still installed — **natives included**, under the lookup the enclosing evaluation is running with
    * ([[Evaluator.currentNativeLookup]]).
    *
    * It passed no lookup at first, on the reasoning that "a stuck native is legitimately stuck and re-firing is the
    * checker's business". Inside a frame there is no later pass to re-fire it in: the frame is gone by the time the
    * checker looks again. So a condition computed by a native — `s == "/api"`, any comparison, any arithmetic — stayed
    * stuck, its `fold` never chose an arm, and the whole frame reduced to a stuck value that read back as a *false*
    * guard rather than as an error. That is the fail-safe direction, which is why it was invisible: the program
    * compiled and simply selected the other implementation. Measured 2026-09-09 against a bodied condition, which
    * reduced where the native one did not.
    *
    * Re-firing can only ever reduce further: [[Evaluator.renormalize]] keeps a native stuck when it does not fire.
    */
  private def settle(v: SemValue): SemValue =
    Evaluator.renormalize(v, MetaStore.empty, Evaluator.currentNativeLookup.value)

  // ----------------------------------------------------------------------------------------------------------------
  // escape
  // ----------------------------------------------------------------------------------------------------------------

  /** `escape(key, body)` — run the thunk under a frame keyed by `key`: `Right(result)`, or `Left(value)` for the
    * `exit` that named this frame. Stuck while the key is not concrete.
    */
  private def escapeNative: SemValue =
    VNative(keyType, key => VNative(VType, body => escape(key, body)))

  private def escape(key: SemValue, body: SemValue): SemValue =
    if (!ConcreteNormalForm.isConcrete(key)) VStuckNative.of(escapeFQN, key, body)
    else {
      val frame = new EscapeFrame(key)
      try {
        val result = inFrame(frame)(settle(Evaluator.applyValue(body, Evaluator.unitValue)))
        // A body that settles to a **neutral** decided nothing: it is waiting on a variable this evaluation has not
        // bound, which is what happens when a definition's own body is reduced before its arguments arrive. Answering
        // `Right(neutral)` there consumes the frame — the escape is gone from the reduced body, and when the neutral is
        // finally re-reduced at a concrete argument, an `exit` inside it has no frame to reach and either sticks or
        // lands in whatever frame happens to be installed then. Staying stuck instead lets the whole escape re-fire
        // once the argument is there, with its frame installed around the part that needs it.
        if (isNeutral(result)) VStuckNative.of(escapeFQN, key, body) else right(result)
      } catch { case Exiting(exited, value) if exited eq frame => left(value) }
    }

  /** Whether a settled value is still waiting on a variable — a neutral head, or one under an application spine. */
  private def isNeutral(v: SemValue): Boolean = v match {
    case _: SemValue.VNeutral => true
    case _                    => false
  }

  /** `exit(key, value)` — leave to the nearest enclosing escape frame keyed by `key`, with `value` settled first (an
    * exit pending inside the payload is taken before this one, as strict evaluation would). Stuck when no such frame
    * is installed.
    */
  private def exitNative: SemValue =
    VNative(keyType, key => VNative(VType, value => exit(key, value)))

  private def exit(key: SemValue, value: SemValue): SemValue = {
    val settled = settle(value)
    nearestEscape(key) match {
      case Some(frame) => throw Exiting(frame, settled)
      case None        => VStuckNative.of(exitFQN, key, settled)
    }
  }

  private def left(value: SemValue): SemValue  = VTopDef(leftFQN, None, Spine.SNil :+ value)
  private def right(value: SemValue): SemValue = VTopDef(rightFQN, None, Spine.SNil :+ value)

  // ----------------------------------------------------------------------------------------------------------------
  // cell
  // ----------------------------------------------------------------------------------------------------------------

  /** `withCell(key, initial, body)` — run the thunk under a cell keyed by `key` holding `initial`; answers
    * `pair(result, finalContent)`. Stuck while the key is not concrete. An exit through the call discards the cell.
    */
  private def withCellNative: SemValue =
    VNative(keyType, key => VNative(VType, initial => VNative(VType, body => withCell(key, initial, body))))

  private def withCell(key: SemValue, initial: SemValue, body: SemValue): SemValue =
    if (!ConcreteNormalForm.isConcrete(key)) VStuckNative.of(withCellFQN, key, initial, body)
    else {
      val frame  = new CellFrame(key, settle(initial))
      val result = inFrame(frame)(settle(Evaluator.applyValue(body, Evaluator.unitValue)))
      pair(result, frame.content)
    }

  /** `read(key)` — the content of the nearest enclosing cell keyed by `key`; stuck when there is none. */
  private def readNative: SemValue = VNative(keyType, key => read(key))

  private def read(key: SemValue): SemValue =
    nearestCell(key).map(_.content).getOrElse(VStuckNative.of(cellReadFQN, key))

  /** `write(key, content)` — replace the content of the nearest enclosing cell keyed by `key`, settling it first;
    * answers unit. Stuck when there is no such cell.
    */
  private def writeNative: SemValue =
    VNative(keyType, key => VNative(VType, content => write(key, content)))

  private def write(key: SemValue, content: SemValue): SemValue =
    nearestCell(key) match {
      case Some(frame) =>
        frame.content = settle(content)
        Evaluator.unitValue
      case None        => VStuckNative.of(cellWriteFQN, key, content)
    }

  private def pair(first: SemValue, second: SemValue): SemValue =
    VTopDef(pairConstructorFQN, None, Spine.SNil :+ first :+ second)
}

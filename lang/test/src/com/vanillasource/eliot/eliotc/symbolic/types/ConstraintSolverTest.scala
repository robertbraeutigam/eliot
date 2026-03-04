package com.vanillasource.eliot.eliotc.symbolic.types

import cats.data.Chain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicUnification.Constraint
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

class ConstraintSolverTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  import ConstraintSolverTest.*

  // --- Concrete value unification ---

  "constraint solver" should "unify identical concrete types" in {
    solve(IntT :=: IntT).asserting(_ shouldBe Map.empty)
  }

  it should "fail on mismatched concrete types" in {
    solveForErrors(IntT :=: StrT).asserting(_ should contain("Type mismatch."))
  }

  // --- Unification variable binding ---

  it should "bind unification var to concrete type (var on left)" in {
    solve(uvar("A") :=: IntT).asserting(_ shouldBe Map("A" -> IntT))
  }

  it should "bind unification var to concrete type (var on right)" in {
    solve(IntT :=: uvar("A")).asserting(_ shouldBe Map("A" -> IntT))
  }

  it should "bind two unification vars together" in {
    solve(uvar("A") :=: uvar("B")).asserting(_ shouldBe Map("A" -> uvar("B")))
  }

  it should "chain unification var bindings" in {
    solve(uvar("A") :=: uvar("B"), uvar("B") :=: IntT).asserting(_ shouldBe Map("A" -> uvar("B"), "B" -> IntT))
  }

  it should "resolve transitive bindings in later constraints" in {
    solve(uvar("A") :=: IntT, uvar("A") :=: uvar("B")).asserting(_ shouldBe Map("A" -> IntT, "B" -> IntT))
  }

  it should "succeed when same var is bound to same concrete type twice" in {
    solve(uvar("A") :=: IntT, uvar("A") :=: IntT).asserting(_ shouldBe Map("A" -> IntT))
  }

  it should "fail when same var is bound to conflicting concrete types" in {
    solveForErrors(uvar("A") :=: IntT, uvar("A") :=: StrT).asserting(_ should contain("Type mismatch."))
  }

  // --- Recursion check ---

  it should "fail on recursion (infinite type)" in {
    val circular = funType(uvar("A"), IntT)
    solveForErrors(uvar("A") :=: circular).asserting(_ should contain("Infinite type detected."))
  }

  it should "fail on recursion (var on right)" in {
    val circular = funType(IntT, uvar("A"))
    solveForErrors(circular :=: uvar("A")).asserting(_ should contain("Infinite type detected."))
  }

  // --- Universal variables ---

  it should "unify identical universal vars" in {
    solveU(uvar("T") :=: uvar("T"))("T").asserting(_ shouldBe Map.empty)
  }

  it should "fail on mismatched universal vars" in {
    solveUForErrors(uvar("T") :=: uvar("U"))("T", "U").asserting(_ should contain("Type mismatch."))
  }

  it should "fail when universal var meets concrete type" in {
    solveUForErrors(uvar("T") :=: IntT)("T").asserting(_ should contain("Type mismatch."))
  }

  it should "fail when concrete type meets universal var" in {
    solveUForErrors(IntT :=: uvar("T"))("T").asserting(_ should contain("Type mismatch."))
  }

  it should "bind unification var to universal var" in {
    solveU(uvar("X") :=: uvar("T"))("T").asserting(_ shouldBe Map("X" -> uvar("T")))
  }

  // --- Function type unification (it's just FunctionApplication case with specialized error messages) ---

  it should "unify identical function types" in {
    solve(funType(IntT, StrT) :=: funType(IntT, StrT)).asserting(_ shouldBe Map.empty)
  }

  it should "fail on function type parameter mismatch" in {
    solveForErrors(funType(IntT, StrT) :=: funType(StrT, StrT))
      .asserting(_ should contain("Parameter type mismatch."))
  }

  it should "fail on function type return mismatch" in {
    solveForErrors(funType(IntT, IntT) :=: funType(IntT, StrT))
      .asserting(_ should contain("Return type mismatch."))
  }

  it should "unify function type with unification vars" in {
    solve(funType(uvar("A"), uvar("B")) :=: funType(IntT, StrT))
      .asserting(_ shouldBe Map("A" -> IntT, "B" -> StrT))
  }

  it should "unify nested function types" in {
    solve(funType(funType(uvar("A"), uvar("B")), uvar("C")) :=: funType(funType(IntT, StrT), IntT))
      .asserting(_ shouldBe Map("A" -> IntT, "B" -> StrT, "C" -> IntT))
  }

  // --- Function application unification ---

  it should "unify identical function applications" in {
    val app = funApp(IntT, StrT)
    solve(app :=: app).asserting(_ shouldBe Map.empty)
  }

  it should "fail on function application target mismatch" in {
    solveForErrors(funApp(IntT, StrT) :=: funApp(StrT, StrT))
      .asserting(_ should contain("Type constructor mismatch."))
  }

  it should "fail on function application argument mismatch" in {
    solveForErrors(funApp(IntT, IntT) :=: funApp(IntT, StrT))
      .asserting(_ should contain("Type argument mismatch."))
  }

  it should "unify function applications with unification vars" in {
    solve(funApp(uvar("A"), uvar("B")) :=: funApp(IntT, StrT))
      .asserting(_ shouldBe Map("A" -> IntT, "B" -> StrT))
  }

  // --- Native function unification ---

  it should "unify native functions with equal parameter types" in {
    val nf = NativeFunction(bigIntType, _ => IntT)
    solve(nf :=: nf).asserting(_ shouldBe Map.empty)
  }

  it should "fail on native functions with different parameter types" in {
    solveForErrors(NativeFunction(bigIntType, _ => IntT) :=: NativeFunction(stringType, _ => StrT))
      .asserting(_ should contain("Type mismatch."))
  }

  // --- Structural mismatches ---

  it should "fail when concrete meets function application" in {
    solveForErrors(IntT :=: funApp(IntT, StrT)).asserting(_ should contain("Type mismatch."))
  }

  it should "fail when function type meets concrete" in {
    solveForErrors(funType(IntT, StrT) :=: IntT).asserting(_ should contain("Type mismatch."))
  }

  // --- Multiple constraints ---

  it should "solve multiple independent constraints" in {
    solve(uvar("A") :=: IntT, uvar("B") :=: StrT)
      .asserting(_ shouldBe Map("A" -> IntT, "B" -> StrT))
  }

  it should "solve constraints that share variables" in {
    solve(funType(uvar("A"), uvar("B")) :=: funType(IntT, uvar("C")), uvar("C") :=: StrT)
      .asserting(_ shouldBe Map("A" -> IntT, "B" -> uvar("C"), "C" -> StrT))
  }

  it should "propagate earlier bindings to later constraints" in {
    solve(uvar("A") :=: IntT, funType(uvar("A"), uvar("B")) :=: funType(IntT, StrT))
      .asserting(_ shouldBe Map("A" -> IntT, "B" -> StrT))
  }

  it should "fail when later constraint conflicts with earlier binding" in {
    solveForErrors(uvar("A") :=: IntT, funType(uvar("A"), StrT) :=: funType(StrT, StrT))
      .asserting(_ should contain("Parameter type mismatch."))
  }

  // --- Custom error messages ---

  it should "use constraint error message for concrete mismatches" in {
    solveForErrors(Seq(Constraint(IntT, s(StrT), "Custom error.")))
      .asserting(_ should contain("Custom error."))
  }
}

object ConstraintSolverTest {
  private val testUri = URI.create("test.els")

  private val IntT: ExpressionValue = ConcreteValue(bigIntType)
  private val StrT: ExpressionValue = ConcreteValue(stringType)

  private def uvar(name: String): ParameterReference = ParameterReference(name, Value.Type)

  private def funType(param: ExpressionValue, ret: ExpressionValue): ExpressionValue =
    ExpressionValue.functionType(param, ret)

  private def funApp(target: ExpressionValue, arg: ExpressionValue): FunctionApplication =
    FunctionApplication(s(target), s(arg))

  private def s[T](value: T): Sourced[T] = Sourced(testUri, PositionRange.zero, value)

  extension (left: ExpressionValue) {
    private def :=:(right: ExpressionValue): Constraint = Constraint(left, s(right), "Type mismatch.")
  }

  private val mockProcess: CompilationProcess = new CompilationProcess {
    private val sourceContent = SourceContent(testUri, Sourced(testUri, PositionRange.zero, "test source"))

    override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): IO[Option[V]] =
      key match {
        case SourceContent.Key(uri) if uri == testUri => IO.pure(Some(sourceContent.asInstanceOf[V]))
        case _                                        => IO.pure(None)
      }

    override def registerFact(value: CompilerFact): IO[Unit] = IO.unit
  }

  private def runSolver(
      constraints: Seq[Constraint],
      universalVars: Set[String] = Set.empty
  ): IO[Either[Seq[CompilerError], Map[String, ExpressionValue]]] =
    ConstraintSolver
      .solve(SymbolicUnification(constraints), universalVars)
      .run(mockProcess)
      .run(Chain.empty)
      .value
      .map {
        case Left(errors)                          => Left(errors.toList)
        case Right((errors, _)) if errors.nonEmpty => Left(errors.toList)
        case Right((_, state))                     => Right(state.substitutions)
      }

  private def solve(constraints: Constraint*): IO[Map[String, ExpressionValue]] =
    runSolver(constraints).map {
      case Right(subs)  => subs
      case Left(errors) => throw new Exception(s"Expected success but got errors: ${errors.map(_.message)}")
    }

  private def solveU(constraints: Constraint*)(universalVars: String*): IO[Map[String, ExpressionValue]] =
    runSolver(constraints, universalVars.toSet).map {
      case Right(subs)  => subs
      case Left(errors) => throw new Exception(s"Expected success but got errors: ${errors.map(_.message)}")
    }

  private def solveForErrors(constraints: Constraint*): IO[Seq[String]] =
    solveForErrors(constraints)

  private def solveForErrors(constraints: Seq[Constraint], universalVars: Set[String] = Set.empty): IO[Seq[String]] =
    runSolver(constraints, universalVars).map {
      case Left(errors) => errors.map(_.message)
      case Right(_)     => throw new Exception("Expected errors but solving succeeded")
    }

  private def solveUForErrors(constraints: Constraint*)(universalVars: String*): IO[Seq[String]] =
    solveForErrors(constraints, universalVars.toSet)
}

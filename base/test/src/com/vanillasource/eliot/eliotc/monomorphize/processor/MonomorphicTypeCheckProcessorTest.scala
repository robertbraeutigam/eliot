package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.symbolic.fact.{TypeCheckedValue, TypedExpression}

class MonomorphicTypeCheckProcessorTest extends ProcessorTest(MonomorphicTypeCheckProcessor()) {
  private val intVfqn    = ValueFQN(testModuleName, "Int")
  private val stringVfqn = ValueFQN(testModuleName, "String")
  private val boolVfqn   = ValueFQN(testModuleName, "Bool")
  private val intType    = Types.dataType(intVfqn)
  private val stringType = Types.dataType(stringVfqn)
  private val boolType   = Types.dataType(boolVfqn)

  private val functionDataTypeVfqn = ValueFQN(ModuleName.systemFunctionModuleName, "Function$DataType")

  private def functionType(paramType: Value, returnType: Value): Value =
    Value.Structure(
      Map(
        "$typeName" -> Value.Direct(functionDataTypeVfqn, Types.fullyQualifiedNameType),
        "A"         -> paramType,
        "B"         -> returnType
      ),
      Value.Type
    )

  /** Create the NamedEvaluable for Function$DataType (a curried NativeFunction). */
  private def functionDataTypeEvaluable: NamedEvaluable =
    NamedEvaluable(
      functionDataTypeVfqn,
      NativeFunction(
        Value.Type,
        paramA =>
          NativeFunction(
            Value.Type,
            paramB =>
              ConcreteValue(
                Value.Structure(
                  Map(
                    "$typeName" -> Value.Direct(functionDataTypeVfqn, Types.fullyQualifiedNameType),
                    "A"         -> paramA,
                    "B"         -> paramB
                  ),
                  Value.Type
                )
              )
          )
      )
    )

  "MonomorphicTypeCheckProcessor" should "monomorphize non-generic value" in {
    // value: Int (no type params, no body)
    val valueVfqn   = ValueFQN(testModuleName, "value")
    val typeChecked = TypeCheckedValue(
      valueVfqn,
      sourced("value"),
      ConcreteValue(Types.dataType(intVfqn)),
      None
    )

    runProcessor(MonomorphicValue.Key(valueVfqn, Seq.empty), Seq(typeChecked))
      .asserting { result =>
        result.vfqn shouldBe valueVfqn
        result.typeArguments shouldBe Seq.empty
        result.signature shouldBe intType
        result.runtime shouldBe None
      }
  }

  it should "monomorphize identity function with Int" in {
    // id[A](a: A): A = a
    val idVfqn = ValueFQN(testModuleName, "id")

    // Signature: [A] A -> A
    val signature = FunctionLiteral(
      "A",
      Value.Type,
      ExpressionValue.functionType(
        ParameterReference("A", Value.Type),
        ParameterReference("A", Value.Type)
      )
    )

    // Body: parameter reference to "a"
    val body = typedExpr(
      ParameterReference("A", Value.Type),
      TypedExpression.ParameterReference(sourced("a"))
    )

    val typeChecked = TypeCheckedValue(
      idVfqn,
      sourced("id"),
      signature,
      Some(sourced(body.expression))
    )

    runProcessor(MonomorphicValue.Key(idVfqn, Seq(intType)), Seq(typeChecked, functionDataTypeEvaluable))
      .asserting { result =>
        result.vfqn shouldBe idVfqn
        result.typeArguments shouldBe Seq(intType)
        result.signature shouldBe functionType(intType, intType)
        result.runtime.isDefined shouldBe true
        result.runtime.get.value match {
          case MonomorphicExpression.ParameterReference(name) =>
            name.value shouldBe "a"
          case other                                          =>
            fail(s"Expected ParameterReference, got $other")
        }
      }
  }

  it should "monomorphize identity function with String" in {
    val idVfqn = ValueFQN(testModuleName, "id")

    val signature = FunctionLiteral(
      "A",
      Value.Type,
      ExpressionValue.functionType(
        ParameterReference("A", Value.Type),
        ParameterReference("A", Value.Type)
      )
    )

    val body = typedExpr(
      ParameterReference("A", Value.Type),
      TypedExpression.ParameterReference(sourced("a"))
    )

    val typeChecked = TypeCheckedValue(
      idVfqn,
      sourced("id"),
      signature,
      Some(sourced(body.expression))
    )

    runProcessor(MonomorphicValue.Key(idVfqn, Seq(stringType)), Seq(typeChecked, functionDataTypeEvaluable))
      .asserting { result =>
        result.signature shouldBe functionType(stringType, stringType)
      }
  }

  it should "monomorphize function with multiple type parameters" in {
    // const[A, B](a: A, b: B): A = a
    val constVfqn = ValueFQN(testModuleName, "const")

    // Signature: [A][B] A -> B -> A
    val signature = FunctionLiteral(
      "A",
      Value.Type,
      FunctionLiteral(
        "B",
        Value.Type,
        ExpressionValue.functionType(
          ParameterReference("A", Value.Type),
          ExpressionValue.functionType(
            ParameterReference("B", Value.Type),
            ParameterReference("A", Value.Type)
          )
        )
      )
    )

    val body = typedExpr(
      ParameterReference("A", Value.Type),
      TypedExpression.ParameterReference(sourced("a"))
    )

    val typeChecked = TypeCheckedValue(
      constVfqn,
      sourced("const"),
      signature,
      Some(sourced(body.expression))
    )

    runProcessor(MonomorphicValue.Key(constVfqn, Seq(intType, stringType)), Seq(typeChecked, functionDataTypeEvaluable))
      .asserting { result =>
        result.signature shouldBe functionType(
          intType,
          functionType(stringType, intType)
        )
      }
  }

  it should "fail on type argument count mismatch" in {
    val idVfqn = ValueFQN(testModuleName, "id")

    val signature = FunctionLiteral(
      "A",
      Value.Type,
      ExpressionValue.functionType(
        ParameterReference("A", Value.Type),
        ParameterReference("A", Value.Type)
      )
    )

    val typeChecked = TypeCheckedValue(
      idVfqn,
      sourced("id"),
      signature,
      None
    )

    // Request with 2 type args, but id only has 1
    runProcessorForError(
      MonomorphicValue.Key(idVfqn, Seq(intType, stringType)),
      Seq(typeChecked, functionDataTypeEvaluable)
    )
      .asserting(_ shouldBe "Type argument count mismatch: expected 1, got 2")
  }

  it should "monomorphize function literal in body" in {
    // f: Int -> Int = (x: Int) -> x
    val fVfqn = ValueFQN(testModuleName, "f")

    val signature = ExpressionValue.functionType(
      ConcreteValue(Types.dataType(intVfqn)),
      ConcreteValue(Types.dataType(intVfqn))
    )

    val paramType = ConcreteValue(Types.dataType(intVfqn))

    val bodyExpr = typedExpr(
      ConcreteValue(Types.dataType(intVfqn)),
      TypedExpression.ParameterReference(sourced("x"))
    )

    val body = typedExpr(
      signature,
      TypedExpression.FunctionLiteral(
        sourced("x"),
        sourced(paramType),
        sourced(bodyExpr)
      )
    )

    val typeChecked = TypeCheckedValue(
      fVfqn,
      sourced("f"),
      signature,
      Some(sourced(body.expression))
    )

    runProcessor(MonomorphicValue.Key(fVfqn, Seq.empty), Seq(typeChecked, functionDataTypeEvaluable))
      .asserting { result =>
        result.signature shouldBe functionType(intType, intType)
        result.runtime.get.value match {
          case MonomorphicExpression.FunctionLiteral(name, paramType, _) =>
            name.value shouldBe "x"
            paramType shouldBe intType
          case other                                                     =>
            fail(s"Expected FunctionLiteral, got $other")
        }
      }
  }

  it should "monomorphize integer literal in body" in {
    val fVfqn = ValueFQN(testModuleName, "f")

    val signature = ConcreteValue(Types.dataType(intVfqn))

    val body = typedExpr(
      ConcreteValue(Types.dataType(intVfqn)),
      TypedExpression.IntegerLiteral(sourced(BigInt(42)))
    )

    val typeChecked = TypeCheckedValue(
      fVfqn,
      sourced("f"),
      signature,
      Some(sourced(body.expression))
    )

    runProcessor(MonomorphicValue.Key(fVfqn, Seq.empty), Seq(typeChecked))
      .asserting { result =>
        result.runtime.get.value match {
          case MonomorphicExpression.IntegerLiteral(n) =>
            n.value shouldBe BigInt(42)
          case other                                   =>
            fail(s"Expected IntegerLiteral, got $other")
        }
      }
  }

  it should "monomorphize string literal in body" in {
    val fVfqn = ValueFQN(testModuleName, "f")

    val signature = ConcreteValue(Types.dataType(stringVfqn))

    val body = typedExpr(
      ConcreteValue(Types.dataType(stringVfqn)),
      TypedExpression.StringLiteral(sourced("hello"))
    )

    val typeChecked = TypeCheckedValue(
      fVfqn,
      sourced("f"),
      signature,
      Some(sourced(body.expression))
    )

    runProcessor(MonomorphicValue.Key(fVfqn, Seq.empty), Seq(typeChecked))
      .asserting { result =>
        result.runtime.get.value match {
          case MonomorphicExpression.StringLiteral(s) =>
            s.value shouldBe "hello"
          case other                                  =>
            fail(s"Expected StringLiteral, got $other")
        }
      }
  }

  it should "monomorphize value reference to non-generic value" in {
    val fVfqn     = ValueFQN(testModuleName, "f")
    val constVfqn = ValueFQN(testModuleName, "constVal")

    // constVal: Int (no body)
    val constTypeChecked = TypeCheckedValue(
      constVfqn,
      sourced("constVal"),
      ConcreteValue(Types.dataType(intVfqn)),
      None
    )

    // f: Int = constVal
    val fSignature = ConcreteValue(Types.dataType(intVfqn))
    val fBody      = typedExpr(
      ConcreteValue(Types.dataType(intVfqn)),
      TypedExpression.ValueReference(sourced(constVfqn))
    )

    val fTypeChecked = TypeCheckedValue(
      fVfqn,
      sourced("f"),
      fSignature,
      Some(sourced(fBody.expression))
    )

    runProcessor(MonomorphicValue.Key(fVfqn, Seq.empty), Seq(fTypeChecked, constTypeChecked))
      .asserting { result =>
        result.runtime.get.value match {
          case MonomorphicExpression.MonomorphicValueReference(name, typeArgs) =>
            name.value shouldBe constVfqn
            typeArgs shouldBe Seq.empty
          case other                                                           =>
            fail(s"Expected MonomorphicValueReference, got $other")
        }
      }
  }

  it should "monomorphize function application" in {
    val fVfqn  = ValueFQN(testModuleName, "f")
    val idVfqn = ValueFQN(testModuleName, "id")

    // id[A](a: A): A (no body for simplicity)
    val idSignature   = FunctionLiteral(
      "A",
      Value.Type,
      ExpressionValue.functionType(
        ParameterReference("A", Value.Type),
        ParameterReference("A", Value.Type)
      )
    )
    val idTypeChecked = TypeCheckedValue(
      idVfqn,
      sourced("id"),
      idSignature,
      None
    )

    // f: Int = id(42) - where id is instantiated with Int
    val fSignature = ConcreteValue(Types.dataType(intVfqn))

    // The application: id(42)
    val idRef  = typedExpr(
      // id's type at call site after unification is Int -> Int
      ExpressionValue.functionType(
        ConcreteValue(Types.dataType(intVfqn)),
        ConcreteValue(Types.dataType(intVfqn))
      ),
      TypedExpression.ValueReference(sourced(idVfqn))
    )
    val argLit = typedExpr(
      ConcreteValue(Types.dataType(intVfqn)),
      TypedExpression.IntegerLiteral(sourced(BigInt(42)))
    )

    val fBody = typedExpr(
      ConcreteValue(Types.dataType(intVfqn)),
      TypedExpression.FunctionApplication(
        sourced(idRef),
        sourced(argLit)
      )
    )

    val fTypeChecked = TypeCheckedValue(
      fVfqn,
      sourced("f"),
      fSignature,
      Some(sourced(fBody.expression))
    )

    runProcessor(MonomorphicValue.Key(fVfqn, Seq.empty), Seq(fTypeChecked, idTypeChecked, functionDataTypeEvaluable))
      .asserting { result =>
        result.runtime.get.value match {
          case MonomorphicExpression.FunctionApplication(target, arg) =>
            target.value.expression match {
              case MonomorphicExpression.MonomorphicValueReference(name, typeArgs) =>
                name.value shouldBe idVfqn
              // Note: type args inference is based on substitution context
              // In this case, id doesn't have its universals in our context
              case other                                                           =>
                fail(s"Expected MonomorphicValueReference in target, got $other")
            }
            arg.value.expression match {
              case MonomorphicExpression.IntegerLiteral(n) =>
                n.value shouldBe BigInt(42)
              case other                                   =>
                fail(s"Expected IntegerLiteral in arg, got $other")
            }
          case other                                                  =>
            fail(s"Expected FunctionApplication, got $other")
        }
      }
  }

  it should "handle direct recursion without infinite loop" in {
    // f: Int -> Int = f (calls itself)
    val fVfqn       = ValueFQN(testModuleName, "f")
    val signature   = ExpressionValue.functionType(
      ConcreteValue(Types.dataType(intVfqn)),
      ConcreteValue(Types.dataType(intVfqn))
    )
    // Body references itself: f calls f
    val fBody       = typedExpr(
      signature,
      TypedExpression.ValueReference(sourced(fVfqn))
    )
    val typeChecked = TypeCheckedValue(
      fVfqn,
      sourced("f"),
      signature,
      Some(sourced(fBody.expression))
    )

    runProcessorWithTimeout(MonomorphicValue.Key(fVfqn, Seq.empty), Seq(typeChecked, functionDataTypeEvaluable))
      .asserting { result =>
        result.vfqn shouldBe fVfqn
        result.runtime.isDefined shouldBe true
        result.runtime.get.value match {
          case MonomorphicExpression.MonomorphicValueReference(name, typeArgs) =>
            name.value shouldBe fVfqn
            typeArgs shouldBe Seq.empty

          case other =>
            fail(s"Expected MonomorphicValueReference, got $other")

        }

      }
  }

  it should "handle mutual recursion without infinite loop" in {
    // f: Int -> Int = g
    // g: Int -> Int = f
    val fVfqn        = ValueFQN(testModuleName, "f")
    val gVfqn        = ValueFQN(testModuleName, "g")
    val signature    = ExpressionValue.functionType(
      ConcreteValue(Types.dataType(intVfqn)),
      ConcreteValue(Types.dataType(intVfqn))
    )
    // f calls g
    val fBody        = typedExpr(
      signature,
      TypedExpression.ValueReference(sourced(gVfqn))
    )
    val fTypeChecked = TypeCheckedValue(
      fVfqn,
      sourced("f"),
      signature,
      Some(sourced(fBody.expression))
    )
    // g calls f
    val gBody        = typedExpr(
      signature,
      TypedExpression.ValueReference(sourced(fVfqn))
    )
    val gTypeChecked = TypeCheckedValue(
      gVfqn,
      sourced("g"),
      signature,
      Some(sourced(gBody.expression))
    )

    runProcessorWithTimeout(
      MonomorphicValue.Key(fVfqn, Seq.empty),
      Seq(fTypeChecked, gTypeChecked, functionDataTypeEvaluable)
    )
      .asserting { result =>
        result.vfqn shouldBe fVfqn
        result.runtime.isDefined shouldBe true

      }
  }

  private def typedExpr(exprType: ExpressionValue, expr: TypedExpression.Expression): TypedExpression =
    TypedExpression(exprType, expr)

  private def runProcessor(
      key: MonomorphicValue.Key,
      facts: Seq[CompilerFact]
  ): IO[MonomorphicValue] =
    runGeneratorWithFacts(facts, key).flatMap { case (result, errors) =>
      if (errors.nonEmpty) IO.raiseError(new Exception(s"Errors: ${errors.map(_.message).mkString(", ")}"))
      else IO.pure(result.getOrElse(throw new Exception("MonomorphicValue not produced")))
    }

  private def runProcessorForError(
      key: MonomorphicValue.Key,
      facts: Seq[CompilerFact]
  ): IO[String] =
    runGeneratorWithFacts(facts, key).map { case (_, errors) =>
      errors.headOption.map(_.message).getOrElse(throw new Exception("Expected error but none occurred"))
    }

  private def runProcessorWithTimeout(
      key: MonomorphicValue.Key,
      facts: Seq[CompilerFact]
  ): IO[MonomorphicValue] = {
    import scala.concurrent.duration.*
    runProcessor(key, facts).timeout(1.seconds)
  }
}

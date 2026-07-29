package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{addMonomorphicDataFieldsAndCtor, mangledMethodName, valueType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.SignatureView
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.uncurry.fact.{MonomorphicParameterDefinition, UncurriedMonomorphicValue}
import com.vanillasource.eliot.eliotc.used.UsedNames.UsageStats

/** The **partial-arity variants of a body-less native**.
  *
  * A native is emitted once, at its declared arity: `substring(BigInteger, BigInteger, String)`. A call site, however,
  * emits its call at whatever arity the application spine has — `directCallArity` in [[ExpressionCodeGenerator]] is
  * the spine length for a native, which has no natural arity to bound it. An Eliot-bodied definition survives that
  * because `JvmClassGenerator.createModuleMethodBody` generates one method per arity the program uses it at, so
  * `take(BigInteger)` and `take(BigInteger, String)` both exist. A native had only the one, and an under-applied call
  * linked to a method that was never emitted: a clean compile and `NoSuchMethodError: 'Function
  * eliot.lang.String.substring(BigInteger, BigInteger)'` at runtime.
  *
  * The gap only opens when the partial application has to become a **value**. `s.substring(0, 3)` is fine, because
  * uncurrying flattens it into one full-arity call and no function is ever built; `def firstThree: String => String =
  * substring(0, 3)`, or an argument hoisted into a `flatMap` chain and `pure`-wrapped, is not.
  *
  * This closes it by emitting the missing variants, one JVM method plus one closure class per level:
  *
  * {{{
  * static Function substring(BigInteger a, BigInteger b) { return new String$substring$partial$2(a, b); }
  *
  * final class String$substring$partial$2 implements Function {   // fields a, b
  *    public Object apply(Object s) { return substring(a, b, (String) s); }
  * }
  * }}}
  *
  * **Levels chain rather than jump.** Each level's `apply` calls the *next* level, which is the real native only at
  * the last one. So a native demanded at arity `k` needs every level in `[k, arity - 1]`, not just `k` — one closure
  * per missing argument, which is also what makes the shape uniform: every frame is a one-argument
  * `java.util.function.Function`, exactly as [[LambdaGenerator]] builds an Eliot lambda, and no N-ary functional
  * interface is invented.
  *
  * **A generic native is refused, loudly.** Those are emitted once erased and called by plain name + erased signature
  * ([[NativeImplementation.genericNativeSignatures]]), and that call descriptor carries the *full* erased parameter
  * list however many arguments the site supplies — so a partial application of one is malformed before it gets here.
  * Rather than emit bytecode that would fail verification, this reports it. No such use exists today; the message is
  * there so the first one is a compile error at its own definition rather than a puzzle at runtime.
  */
object NativePartialApplication {

  /** Emit every partial-arity variant `vfqn` is used at but does not have. Empty when the native is only ever fully
    * applied, which is the common case and costs one signature read.
    */
  def generate(
      mainClassGenerator: ClassGenerator,
      vfqn: ValueFQN,
      stats: UsageStats
  ): CompilerIO[Seq[ClassFile]] =
    for {
      resolved   <- getFactOrAbort(OperatorResolvedValue.Key(vfqn))
      fullArity   = SignatureView.of(resolved.signature).parameters.length
      partialArities = stats.directCallApplications.keys.filter(_ < fullArity).toSeq
      files      <- partialArities.minOption match {
                      case None           => Seq.empty[ClassFile].pure[CompilerIO]
                      case Some(lowest)   =>
                        if (NativeImplementation.genericNativeSignatures.contains(vfqn))
                          compilerAbort[Seq[ClassFile]](
                            resolved.name.as(
                              s"This generic native is used with $lowest of its $fullArity arguments; a generic " +
                                "native is emitted once erased and can only be called fully applied. Apply all its " +
                                "arguments at the call site, or wrap it in an ordinary definition."
                            )
                          )
                        else generateLevels(mainClassGenerator, vfqn, stats, lowest, fullArity)
                    }
    } yield files

  /** One level per missing argument, for each instantiation the native is used at. Deduplicated by emitted method
    * name: two instantiations whose representations coincide give the same method, exactly as
    * `JvmClassGenerator.createModuleMethodBody` deduplicates its own.
    */
  private def generateLevels(
      mainClassGenerator: ClassGenerator,
      vfqn: ValueFQN,
      stats: UsageStats,
      lowestArity: Int,
      fullArity: Int
  ): CompilerIO[Seq[ClassFile]] = {
    val instantiations = stats.monomorphicTypeParameters.distinct match {
      case Seq() => Seq(Seq.empty[GroundValue])
      case xs    => xs
    }

    instantiations
      .flatMap(typeArgs => Range(lowestArity, fullArity).map(typeArgs -> _))
      .foldLeftM((Seq.empty[ClassFile], Set.empty[String])) { case ((files, seen), (typeArgs, level)) =>
        val name = levelName(vfqn, typeArgs, level)
        if (seen.contains(name)) (files, seen).pure[CompilerIO]
        else generateLevel(mainClassGenerator, vfqn, typeArgs, level).map(f => (files :+ f, seen + name))
      }
      .map(_._1)
  }

  private def levelName(vfqn: ValueFQN, typeArgs: Seq[GroundValue], level: Int): String =
    mangledMethodName(vfqn, typeArgs) + "$partial$" + level

  /** The method + closure class for one level: `name(p1..pj)` returning a `Function` that holds `p1..pj` and, applied,
    * calls `name(p1..pj, x)` — the next level, or the native itself when `j + 1` is its full arity.
    */
  private def generateLevel(
      mainClassGenerator: ClassGenerator,
      vfqn: ValueFQN,
      typeArgs: Seq[GroundValue],
      level: Int
  ): CompilerIO[ClassFile] =
    for {
      atLevel  <- getFactOrAbort(UncurriedMonomorphicValue.Key(vfqn, typeArgs, level))
      atNext   <- getFactOrAbort(UncurriedMonomorphicValue.Key(vfqn, typeArgs, level + 1))
      captured  = atLevel.parameters
      nextParam = atNext.parameters.last
      calleeFqn = ValueFQN(vfqn.moduleName, QualifiedName(mangledMethodName(vfqn, typeArgs), Qualifier.Default))
      closureFqn = ValueFQN(vfqn.moduleName, QualifiedName(levelName(vfqn, typeArgs, level), Qualifier.Default))
      _        <- mainClassGenerator
                    .createMethod[CompilerIO](
                      JvmIdentifier.encode(mangledMethodName(vfqn, typeArgs)),
                      captured.map(p => valueType(p.parameterType)),
                      valueType(atLevel.returnType)
                    )
                    .use { methodGenerator =>
                      for {
                        _ <- methodGenerator.addNew[CompilerIO](closureFqn)
                        _ <- captured.zipWithIndex.traverse_ { case (parameter, index) =>
                               methodGenerator.addLoadVar[CompilerIO](valueType(parameter.parameterType), index)
                             }
                        _ <- methodGenerator.addCallToCtor[CompilerIO](
                               closureFqn,
                               captured.map(p => valueType(p.parameterType))
                             )
                      } yield ()
                    }
      closure  <- generateClosure(mainClassGenerator, closureFqn, calleeFqn, captured, nextParam, atNext.returnType)
    } yield closure

  private def generateClosure(
      mainClassGenerator: ClassGenerator,
      closureFqn: ValueFQN,
      calleeFqn: ValueFQN,
      captured: Seq[MonomorphicParameterDefinition],
      nextParam: MonomorphicParameterDefinition,
      nextReturn: GroundValue
  ): CompilerIO[ClassFile] =
    for {
      closureGenerator <- mainClassGenerator.createInnerClassGenerator[CompilerIO](
                            JvmIdentifier.encode(closureFqn.name.name),
                            Seq("java/util/function/Function")
                          )
      _                <- closureGenerator.addMonomorphicDataFieldsAndCtor[CompilerIO](captured)
      _                <- closureGenerator
                            .createApplyMethod[CompilerIO](
                              Seq(valueType(nextParam.parameterType)),
                              valueType(nextReturn)
                            )
                            .use { applyGenerator =>
                              for {
                                _ <- captured.traverse_ { parameter =>
                                       applyGenerator.addLoadVar[CompilerIO](closureFqn, 0) >>
                                         applyGenerator.addGetField[CompilerIO](
                                           JvmIdentifier.encode(parameter.name.value),
                                           valueType(parameter.parameterType),
                                           closureFqn
                                         )
                                     }
                                _ <- applyGenerator.addLoadVar[CompilerIO](valueType(nextParam.parameterType), 1)
                                _ <- applyGenerator.addCastTo[CompilerIO](valueType(nextParam.parameterType))
                                _ <- applyGenerator.addCallTo[CompilerIO](
                                       calleeFqn,
                                       (captured :+ nextParam).map(p => valueType(p.parameterType)),
                                       valueType(nextReturn)
                                     )
                              } yield ()
                            }
      classFile        <- closureGenerator.generate[CompilerIO]()
    } yield classFile
}

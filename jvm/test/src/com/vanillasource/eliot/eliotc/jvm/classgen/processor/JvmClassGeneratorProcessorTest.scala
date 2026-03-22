package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.compiler.FactGenerator
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.eval.processor.{DataTypeEvaluator, ExistingNamedValueEvaluator, SystemValueEvaluator}
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.implementation.processor.{AbilityImplementationCheckProcessor, AbilityImplementationProcessor}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.GeneratedModule
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.module.processor.{ModuleNamesProcessor, ModuleValueProcessor, UnifiedModuleNamesProcessor, UnifiedModuleValueProcessor}
import com.vanillasource.eliot.eliotc.monomorphize.processor.MonomorphicTypeCheckProcessor
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.uncurry.processor.MonomorphicUncurryingProcessor
import com.vanillasource.eliot.eliotc.used.UsedNamesProcessor
import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI
import java.nio.file.Path

class JvmClassGeneratorProcessorTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val file           = URI.create("Test.els")
  private val testModuleName = ModuleName(Seq.empty, "Test")
  private val mainVfqn       = ValueFQN(testModuleName, QualifiedName("main", Qualifier.Default))
  private val moduleKey      = GeneratedModule.Key(testModuleName, mainVfqn)

  private val processors = SequentialCompilerProcessors(
    Seq(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      SystemValueEvaluator(),
      ExistingNamedValueEvaluator(),
      DataTypeEvaluator(),
      ModuleNamesProcessor(),
      ModuleValueProcessor(Seq(ModuleName.systemFunctionModuleName, ModuleName(ModuleName.defaultSystemPackage, "Unit"), ModuleName(ModuleName.defaultSystemPackage, "PatternMatch"), ModuleName(ModuleName.defaultSystemPackage, "TypeMatch"))),
      UnifiedModuleNamesProcessor(),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      MatchDesugaringProcessor(),
      OperatorResolverProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      MonomorphicTypeCheckProcessor(),
      UsedNamesProcessor(),
      MonomorphicUncurryingProcessor(),
      JvmClassGenerator()
    )
  )

  private def runGenerator(
      source: String,
      trigger: CompilerFactKey[? <: CompilerFact]
  ): IO[(Seq[CompilerError], Map[CompilerFactKey[?], CompilerFact])] =
    for {
      generator <- FactGenerator.create(processors)
      _         <- generator.registerFact(SourceContent(file, Sourced(file, PositionRange.zero, source)))
      _         <- generator.registerFact(PathScan(Path.of("Test.els"), Seq(file)))
      _         <- Seq("Function" -> "opaque type Function[A, B]", "Type" -> "opaque type Type", "Unit" -> "opaque type Unit", "PatternMatch" -> "ability PatternMatch[T] {\ntype Cases[R]\ndef handleCases[R](value: T, cases: Cases[R]): R\n}", "TypeMatch" -> "ability TypeMatch[T] {\ntype Fields[R]\ndef typeMatch[R](value: Type, matched: Fields[R], notMatched: Function[Unit, R]): R\n}").traverse { (module, content) =>
                     val impFile = URI.create(s"eliot/lang/$module.els")
                     generator.registerFact(PathScan(Path.of(s"eliot/lang/$module.els"), Seq(impFile))) >>
                       generator.registerFact(SourceContent(impFile, Sourced(impFile, PositionRange.zero, content)))
                   }
      _         <- generator.getFact(trigger)
      facts     <- generator.currentFacts()
      errors    <- generator.currentErrors()
    } yield (errors, facts)

  "jvm class generator" should "resolve ability call directly via monomorphization" in {
    runGenerator(
      """ability Show[A] { def show(x: A): A }
        |data MyStr
        |implement Show[MyStr] { def show(x: MyStr): MyStr = x }
        |def someValue: MyStr = someValue
        |def main: MyStr = f(someValue)
        |def f[A ~ Show[A]](x: A): A = show(x)""".stripMargin,
      moduleKey
    ).map { case (_, facts) =>
      val generatedModule = facts(moduleKey).asInstanceOf[GeneratedModule]
      val testClassBytes  = generatedModule.classFiles.find(_.fileName == "Test.class").get.bytecode
      val classReader     = new ClassReader(testClassBytes)
      var fCallsShowImpl  = false
      classReader.accept(
        new ClassVisitor(Opcodes.ASM9) {
          override def visitMethod(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              exceptions: Array[String]
          ): MethodVisitor =
            if (name == "f$MyStr")
              new MethodVisitor(Opcodes.ASM9) {
                override def visitMethodInsn(
                    opcode: Int,
                    owner: String,
                    name: String,
                    descriptor: String,
                    isInterface: Boolean
                ): Unit =
                  if (opcode == Opcodes.INVOKESTATIC && name == "show")
                    fCallsShowImpl = true
              }
            else null
        },
        0
      )
      fCallsShowImpl
    }.asserting(_ shouldBe true)
  }

  it should "generate monomorphically mangled method name for generic function" in {
    runGenerator(
      """ability Show[A] { def show(x: A): A }
        |data MyStr
        |implement Show[MyStr] { def show(x: MyStr): MyStr = x }
        |def someValue: MyStr = someValue
        |def main: MyStr = f(someValue)
        |def f[A ~ Show[A]](x: A): A = x""".stripMargin,
      moduleKey
    ).map { case (_, facts) =>
      val generatedModule = facts(moduleKey).asInstanceOf[GeneratedModule]
      val testClassBytes  = generatedModule.classFiles.find(_.fileName == "Test.class").get.bytecode
      val classReader     = new ClassReader(testClassBytes)
      var fDescriptor     = Option.empty[String]
      classReader.accept(
        new ClassVisitor(Opcodes.ASM9) {
          override def visitMethod(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              exceptions: Array[String]
          ): MethodVisitor = {
            if (name == "f$MyStr") fDescriptor = Some(descriptor)
            null
          }
        },
        0
      )
      fDescriptor
    }.asserting(_ shouldBe Some("(LTest$MyStr;)LTest$MyStr;"))
  }

  it should "generate monomorphic methods for generic function" in {
    runGenerator(
      """data MyStr
        |def someValue: MyStr = someValue
        |def id[A](x: A): A = x
        |def main: MyStr = id(someValue)""".stripMargin,
      moduleKey
    ).map { case (errors, facts) =>
      val generatedModule    = facts.get(moduleKey).map(_.asInstanceOf[GeneratedModule]).getOrElse(
        fail(s"GeneratedModule not found. Errors: ${errors.map(e => s"${e.contentSource}: ${e.message}").mkString("; ")}")
      )
      val testClassBytes     = generatedModule.classFiles.find(_.fileName == "Test.class").get.bytecode
      val classReader        = new ClassReader(testClassBytes)
      var idMethods          = Map.empty[String, String]
      classReader.accept(
        new ClassVisitor(Opcodes.ASM9) {
          override def visitMethod(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              exceptions: Array[String]
          ): MethodVisitor = {
            if (name.startsWith("id")) idMethods = idMethods + (name -> descriptor)
            null
          }
        },
        0
      )
      idMethods
    }.asserting(_ shouldBe Map(
      "id$MyStr" -> "(LTest$MyStr;)LTest$MyStr;"
    ))
  }

  it should "generate separate monomorphic methods for different type args" in {
    runGenerator(
      """data MyStr
        |data MyInt
        |def someStr: MyStr = someStr
        |def someInt: MyInt = someInt
        |def id[A](x: A): A = x
        |def main: MyStr = id(id(someStr))""".stripMargin,
      moduleKey
    ).map { case (_, facts) =>
      val generatedModule    = facts(moduleKey).asInstanceOf[GeneratedModule]
      val testClassBytes     = generatedModule.classFiles.find(_.fileName == "Test.class").get.bytecode
      val classReader        = new ClassReader(testClassBytes)
      var idMethods          = Set.empty[String]
      classReader.accept(
        new ClassVisitor(Opcodes.ASM9) {
          override def visitMethod(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              exceptions: Array[String]
          ): MethodVisitor = {
            if (name.startsWith("id")) idMethods = idMethods + name
            null
          }
        },
        0
      )
      idMethods
    }.asserting(_ shouldBe Set("id$MyStr"))
  }
}

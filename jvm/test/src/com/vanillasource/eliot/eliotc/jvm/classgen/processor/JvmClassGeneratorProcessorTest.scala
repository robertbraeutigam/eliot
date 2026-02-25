package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.abilitycheck.AbilityCheckProcessor
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
import com.vanillasource.eliot.eliotc.operator.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan
import com.vanillasource.eliot.eliotc.symbolic.processor.SymbolicTypeCheckProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.uncurry.processor.UncurryingProcessor
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
      ModuleValueProcessor(Seq(ModuleName.systemFunctionModuleName)),
      UnifiedModuleNamesProcessor(),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      OperatorResolverProcessor(),
      SymbolicTypeCheckProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      AbilityCheckProcessor(),
      MonomorphicTypeCheckProcessor(),
      UsedNamesProcessor(),
      UncurryingProcessor(),
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
      _         <- Seq("Function" -> "data Function[A, B]").traverse { (module, content) =>
                     val impFile = URI.create(s"eliot/lang/$module.els")
                     generator.registerFact(PathScan(Path.of(s"eliot/lang/$module.els"), Seq(impFile))) >>
                       generator.registerFact(SourceContent(impFile, Sourced(impFile, PositionRange.zero, content)))
                   }
      _         <- generator.getFact(trigger)
      facts     <- generator.currentFacts()
      errors    <- generator.currentErrors()
    } yield (errors, facts)

  "jvm class generator" should "inject concrete dictionary singleton at call site" in {
    runGenerator(
      """ability Show[A] { def show(x: A): A }
        |data MyStr
        |implement Show[MyStr] { def show(x: MyStr): MyStr = x }
        |def someValue: MyStr = someValue
        |def main: MyStr = g(someValue)
        |def g(x: MyStr): MyStr = f(x)
        |def f[A ~ Show[A]](x: A): A = show(x)""".stripMargin,
      moduleKey
    ).map { case (errors, facts) =>
      val generatedModule  = facts(moduleKey).asInstanceOf[GeneratedModule]
      val testClassBytes   = generatedModule.classFiles.find(_.fileName == "Test.class").get.bytecode
      val classReader      = new ClassReader(testClassBytes)
      var gCallsFWithDict  = false
      classReader.accept(
        new ClassVisitor(Opcodes.ASM9) {
          override def visitMethod(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              exceptions: Array[String]
          ): MethodVisitor =
            if (name == "g")
              new MethodVisitor(Opcodes.ASM9) {
                override def visitMethodInsn(
                    opcode: Int,
                    owner: String,
                    name: String,
                    descriptor: String,
                    isInterface: Boolean
                ): Unit =
                  if (opcode == Opcodes.INVOKESTATIC && name == "f")
                    gCallsFWithDict =
                      descriptor == "(LTest$Show$vtable;Ljava/lang/Object;)Ljava/lang/Object;"
              }
            else null
        },
        0
      )
      gCallsFWithDict
    }.asserting(_ shouldBe true)
  }

  it should "pass through dictionary for generic call site" in {
    runGenerator(
      """ability Show[A] { def show(x: A): A }
        |data MyStr
        |implement Show[MyStr] { def show(x: MyStr): MyStr = x }
        |def someValue: MyStr = someValue
        |def main: MyStr = g(someValue)
        |def g(x: MyStr): MyStr = h(x)
        |def h[A ~ Show[A]](x: A): A = f(x)
        |def f[A ~ Show[A]](x: A): A = show(x)""".stripMargin,
      moduleKey
    ).map { case (errors, facts) =>
      val generatedModule  = facts(moduleKey).asInstanceOf[GeneratedModule]
      val testClassBytes   = generatedModule.classFiles.find(_.fileName == "Test.class").get.bytecode
      val classReader      = new ClassReader(testClassBytes)
      var hCallsFWithDict  = false
      classReader.accept(
        new ClassVisitor(Opcodes.ASM9) {
          override def visitMethod(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              exceptions: Array[String]
          ): MethodVisitor =
            if (name == "h")
              new MethodVisitor(Opcodes.ASM9) {
                override def visitMethodInsn(
                    opcode: Int,
                    owner: String,
                    name: String,
                    descriptor: String,
                    isInterface: Boolean
                ): Unit =
                  if (opcode == Opcodes.INVOKESTATIC && name == "f")
                    hCallsFWithDict =
                      descriptor == "(LTest$Show$vtable;Ljava/lang/Object;)Ljava/lang/Object;"
              }
            else null
        },
        0
      )
      hCallsFWithDict
    }.asserting(_ shouldBe true)
  }

  it should "dispatch ability call through dictionary with INVOKEINTERFACE" in {
    runGenerator(
      """ability Show[A] { def show(x: A): A }
        |data MyStr
        |implement Show[MyStr] { def show(x: MyStr): MyStr = x }
        |def someValue: MyStr = someValue
        |def main: MyStr = f(someValue)
        |def f[A ~ Show[A]](x: A): A = show(x)""".stripMargin,
      moduleKey
    ).map { case (_, facts) =>
      val generatedModule      = facts(moduleKey).asInstanceOf[GeneratedModule]
      val testClassBytes       = generatedModule.classFiles.find(_.fileName == "Test.class").get.bytecode
      val classReader          = new ClassReader(testClassBytes)
      var invokeInterfaceOwner = Option.empty[String]
      classReader.accept(
        new ClassVisitor(Opcodes.ASM9) {
          override def visitMethod(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              exceptions: Array[String]
          ): MethodVisitor =
            if (name == "f")
              new MethodVisitor(Opcodes.ASM9) {
                override def visitMethodInsn(
                    opcode: Int,
                    owner: String,
                    name: String,
                    descriptor: String,
                    isInterface: Boolean
                ): Unit =
                  if (opcode == Opcodes.INVOKEINTERFACE) invokeInterfaceOwner = Some(owner)
              }
            else null
        },
        0
      )
      invokeInterfaceOwner
    }.asserting(_ shouldBe Some("Test$Show$vtable"))
  }

  it should "add a dictionary parameter for a constrained function" in {
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
            if (name == "f") fDescriptor = Some(descriptor)
            null
          }
        },
        0
      )
      fDescriptor
    }.asserting(_ shouldBe Some("(LTest$Show$vtable;Ljava/lang/Object;)Ljava/lang/Object;"))
  }
}

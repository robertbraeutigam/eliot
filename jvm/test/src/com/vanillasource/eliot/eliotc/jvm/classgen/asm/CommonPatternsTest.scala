package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.{Types, Value}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.*
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{systemAnyValue, systemFunctionValue, systemUnitValue}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType
import com.vanillasource.eliot.eliotc.uncurry.fact.ParameterDefinition

import java.net.URI

class CommonPatternsTest extends BytecodeTest {
  private val testModule = ModuleName(Seq("test", "pkg"), "TestClass")
  private val stringType = NativeType.systemLangType("String")
  private val anyType    = systemAnyValue
  private val testUri    = URI.create("file:///test.els")
  private val zeroRange  = PositionRange.zero

  private def sourced[T](value: T): Sourced[T] = Sourced(testUri, zeroRange, value)

  private def stringSymbolicType: SymbolicType =
    SymbolicType.TypeReference(ValueFQN(ModuleName(Seq("eliot", "lang"), "String"), QualifiedName("String", Qualifier.Default)))

  private def anySymbolicType: SymbolicType =
    SymbolicType.TypeReference(ValueFQN(ModuleName(Seq("eliot", "lang"), "Any"), QualifiedName("Any", Qualifier.Default)))

  private def functionSymbolicType: SymbolicType =
    SymbolicType.functionType(stringSymbolicType, stringSymbolicType)

  "addDataFieldsAndCtor" should "generate a class with a single field and constructor" in {
    val fields = Seq(ParameterDefinition(sourced("name"), stringSymbolicType))

    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.addDataFieldsAndCtor[IO](fields)
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz = cl.loadClass("test.pkg.TestClass")
                     val ctor  = clazz.getConstructor(classOf[String])
                     val obj   = ctor.newInstance("Alice")
                     val field = clazz.getField("name")
                     print(field.get(obj))
                   }
    } yield output shouldBe "Alice"
  }

  it should "generate a class with multiple fields" in {
    val fields = Seq(
      ParameterDefinition(sourced("first"), stringSymbolicType),
      ParameterDefinition(sourced("second"), stringSymbolicType)
    )

    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.addDataFieldsAndCtor[IO](fields)
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz = cl.loadClass("test.pkg.TestClass")
                     val ctor  = clazz.getConstructor(classOf[String], classOf[String])
                     val obj   = ctor.newInstance("hello", "world")
                     val f1    = clazz.getField("first").get(obj)
                     val f2    = clazz.getField("second").get(obj)
                     print(s"$f1 $f2")
                   }
    } yield output shouldBe "hello world"
  }

  it should "generate a class with no fields and a no-arg constructor" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.addDataFieldsAndCtor[IO](Seq.empty)
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz = cl.loadClass("test.pkg.TestClass")
                     val ctor  = clazz.getConstructor()
                     val obj   = ctor.newInstance()
                     print(obj.getClass.getName)
                   }
    } yield output shouldBe "test.pkg.TestClass"
  }

  it should "generate fields accessible after construction" in {
    val fields = Seq(
      ParameterDefinition(sourced("a"), stringSymbolicType),
      ParameterDefinition(sourced("b"), stringSymbolicType),
      ParameterDefinition(sourced("c"), stringSymbolicType)
    )

    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.addDataFieldsAndCtor[IO](fields)
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz = cl.loadClass("test.pkg.TestClass")
                     val ctor  = clazz.getConstructor(classOf[String], classOf[String], classOf[String])
                     val obj   = ctor.newInstance("x", "y", "z")
                     val a     = clazz.getField("a").get(obj)
                     val b     = clazz.getField("b").get(obj)
                     val c     = clazz.getField("c").get(obj)
                     print(s"$a$b$c")
                   }
    } yield output shouldBe "xyz"
  }

  it should "generate a data class used as an inner class" in {
    val fields = Seq(ParameterDefinition(sourced("value"), stringSymbolicType))

    for {
      cg        <- createClassGenerator[IO](testModule)
      innerCg   <- cg.createInnerClassGenerator[IO](JvmIdentifier("Data"))
      _         <- innerCg.addDataFieldsAndCtor[IO](fields)
      boxType    = ValueFQN(testModule, QualifiedName("Data", Qualifier.Default))
      _         <- cg.createMethod[IO](JvmIdentifier("wrap"), Seq(stringType), anyType).use { mg =>
                     for {
                       _ <- mg.addNew[IO](boxType)
                       _ <- mg.addLoadVar[IO](stringType, 0)
                       _ <- mg.addCallToCtor[IO](boxType, Seq(stringType))
                     } yield ()
                   }
      classFile <- cg.generate[IO]()
      innerFile <- innerCg.generate[IO]()
      output    <- runClasses(Seq(classFile, innerFile)) { cl =>
                     val clazz = cl.loadClass("test.pkg.TestClass")
                     val wrap  = clazz.getMethod("wrap", classOf[String])
                     val obj   = wrap.invoke(null, "inner data")
                     val inner = cl.loadClass("test.pkg.TestClass$Data")
                     print(inner.getField("value").get(obj))
                   }
    } yield output shouldBe "inner data"
  }

  it should "generate a data class with a Function-typed field" in {
    val fields = Seq(ParameterDefinition(sourced("fn"), functionSymbolicType))

    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.addDataFieldsAndCtor[IO](fields)
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz = cl.loadClass("test.pkg.TestClass")
                     val ctor  = clazz.getConstructor(classOf[java.util.function.Function[?, ?]])
                     val fn: java.util.function.Function[Object, Object] = (x: Object) => s"called with $x"
                     val obj   = ctor.newInstance(fn)
                     val field = clazz.getField("fn").get(obj).asInstanceOf[java.util.function.Function[Object, Object]]
                     print(field.apply("test"))
                   }
    } yield output shouldBe "called with test"
  }

  "simpleType" should "return String type for a String TypeReference" in {
    simpleType(stringSymbolicType) shouldBe NativeType.systemLangType("String")
  }

  it should "return Function type for a function expression" in {
    simpleType(functionSymbolicType) shouldBe systemFunctionValue
  }

  it should "return Any type for unsupported expression types" in {
    val literalType = SymbolicType.LiteralType(42, ValueFQN(ModuleName(Seq("eliot", "lang"), "Number"), QualifiedName("Int", Qualifier.Default)))
    simpleType(literalType) shouldBe systemAnyValue
  }

  it should "return the Type type for a TypeVariable" in {
    val typeVar = SymbolicType.TypeVariable("x")
    simpleType(typeVar) shouldBe NativeType.systemTypeValue
  }

  it should "strip DataType suffix from type references" in {
    val dataTypeFqn = ValueFQN(ModuleName(Seq("test"), "Foo"), QualifiedName("Foo", Qualifier.Type))
    val expr        = SymbolicType.TypeReference(dataTypeFqn)
    simpleType(expr) shouldBe ValueFQN(ModuleName(Seq("test"), "Foo"), QualifiedName("Foo", Qualifier.Default))
  }

  it should "strip leading type applications before resolving type" in {
    val innerType = SymbolicType.TypeReference(ValueFQN(ModuleName(Seq("eliot", "lang"), "String"), QualifiedName("String", Qualifier.Default)))
    val applied   = SymbolicType.TypeApplication(sourced(innerType), sourced(stringSymbolicType))
    simpleType(applied) shouldBe NativeType.systemLangType("String")
  }

  "valueToValueFQN" should "extract ValueFQN from a data type Value" in {
    val fqn   = ValueFQN(ModuleName(Seq("test"), "Foo"), QualifiedName("Foo", Qualifier.Default))
    val value = Types.dataType(fqn)
    valueToValueFQN(value) shouldBe fqn
  }

  it should "return Any for Value.Type" in {
    valueToValueFQN(Value.Type) shouldBe systemAnyValue
  }

  it should "return Any for a Structure without $typeName" in {
    val value = Value.Structure(Map("other" -> Value.Direct(42, Value.Type)), Value.Type)
    valueToValueFQN(value) shouldBe systemAnyValue
  }

  it should "return Any for a Direct value" in {
    val value = Value.Direct("something", Value.Type)
    valueToValueFQN(value) shouldBe systemAnyValue
  }
}

package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{systemAnyValue, systemFunctionValue, systemUnitValue}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import org.objectweb.asm.Opcodes

class ClassGeneratorTest extends BytecodeTest {
  private val testModule = ModuleName(Seq("test", "pkg"), "TestClass")
  private val stringType = NativeType.systemLangType("String")
  private val anyType    = systemAnyValue

  "class generator" should "generate a class that can be loaded" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz = cl.loadClass("test.pkg.TestClass")
                     print(clazz.getName)
                   }
    } yield output shouldBe "test.pkg.TestClass"
  }

  it should "generate a class with a main method that can be invoked" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createMainMethod[IO]().use { mg =>
                     mg.runNative[IO] { mv =>
                       mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
                       mv.visitLdcInsn("hello from main")
                       mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "print", "(Ljava/lang/String;)V", false)
                     }
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz  = cl.loadClass("test.pkg.TestClass")
                     val method = clazz.getMethod("main", classOf[Array[String]])
                     method.invoke(null, Array.empty[String])
                   }
    } yield output shouldBe "hello from main"
  }

  it should "generate a static method that returns a string" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createMethod[IO]("greet", Seq.empty, stringType).use { mg =>
                     mg.addLdcInsn[IO]("hi there")
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz  = cl.loadClass("test.pkg.TestClass")
                     val method = clazz.getMethod("greet")
                     print(method.invoke(null))
                   }
    } yield output shouldBe "hi there"
  }

  it should "generate a static method with parameters" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createMethod[IO]("echo", Seq(stringType), stringType).use { mg =>
                     mg.addLoadVar[IO](stringType, 0)
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz  = cl.loadClass("test.pkg.TestClass")
                     val method = clazz.getMethod("echo", classOf[String])
                     print(method.invoke(null, "echoed"))
                   }
    } yield output shouldBe "echoed"
  }

  it should "generate multiple methods in the same class" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createMethod[IO]("first", Seq.empty, stringType).use { mg =>
                     mg.addLdcInsn[IO]("one")
                   }
      _         <- cg.createMethod[IO]("second", Seq.empty, stringType).use { mg =>
                     mg.addLdcInsn[IO]("two")
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz = cl.loadClass("test.pkg.TestClass")
                     val r1    = clazz.getMethod("first").invoke(null)
                     val r2    = clazz.getMethod("second").invoke(null)
                     print(s"$r1,$r2")
                   }
    } yield output shouldBe "one,two"
  }

  it should "generate methods that call other static methods" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createMethod[IO]("inner", Seq.empty, stringType).use { mg =>
                     mg.addLdcInsn[IO]("from inner")
                   }
      _         <- cg.createMethod[IO]("outer", Seq.empty, stringType).use { mg =>
                     val innerVfqn = ValueFQN(testModule, QualifiedName("inner", Qualifier.Default))
                     mg.addCallTo[IO](innerVfqn, Seq.empty, stringType)
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz  = cl.loadClass("test.pkg.TestClass")
                     val method = clazz.getMethod("outer")
                     print(method.invoke(null))
                   }
    } yield output shouldBe "from inner"
  }

  it should "generate a method that passes arguments to another method" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createMethod[IO]("identity", Seq(stringType), stringType).use { mg =>
                     mg.addLoadVar[IO](stringType, 0)
                   }
      _         <- cg.createMethod[IO]("callIdentity", Seq.empty, stringType).use { mg =>
                     val identityVfqn = ValueFQN(testModule, QualifiedName("identity", Qualifier.Default))
                     mg.addLdcInsn[IO]("passed") >>
                       mg.addCallTo[IO](identityVfqn, Seq(stringType), stringType)
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz  = cl.loadClass("test.pkg.TestClass")
                     val method = clazz.getMethod("callIdentity")
                     print(method.invoke(null))
                   }
    } yield output shouldBe "passed"
  }

  it should "generate an inner class that can be loaded" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      innerCg   <- cg.createInnerClassGenerator[IO]("Inner")
      _         <- innerCg.createMethod[IO]("value", Seq.empty, stringType).use { mg =>
                     mg.addLdcInsn[IO]("inner value")
                   }
      classFile <- cg.generate[IO]()
      innerFile <- innerCg.generate[IO]()
      output    <- runClasses(Seq(classFile, innerFile)) { cl =>
                     val clazz  = cl.loadClass("test.pkg.TestClass$Inner")
                     val method = clazz.getMethod("value")
                     print(method.invoke(null))
                   }
    } yield output shouldBe "inner value"
  }

  it should "generate a class with fields and a constructor" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createField[IO]("name", stringType)
      _         <- cg.createCtor[IO](Seq(stringType)).use { mg =>
                     for {
                       _ <- mg.addLoadThis[IO]()
                       _ <- mg.addCallToObjectCtor[IO]()
                       _ <- mg.addLoadThis[IO]()
                       _ <- mg.addLoadVar[IO](stringType, 1)
                       _ <- mg.addPutField[IO]("name", stringType)
                     } yield ()
                   }
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

  it should "generate a constructor with multiple fields" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createField[IO]("first", stringType)
      _         <- cg.createField[IO]("second", stringType)
      _         <- cg.createCtor[IO](Seq(stringType, stringType)).use { mg =>
                     for {
                       _ <- mg.addLoadThis[IO]()
                       _ <- mg.addCallToObjectCtor[IO]()
                       _ <- mg.addLoadThis[IO]()
                       _ <- mg.addLoadVar[IO](stringType, 1)
                       _ <- mg.addPutField[IO]("first", stringType)
                       _ <- mg.addLoadThis[IO]()
                       _ <- mg.addLoadVar[IO](stringType, 2)
                       _ <- mg.addPutField[IO]("second", stringType)
                     } yield ()
                   }
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

  it should "generate a class implementing an interface" in {
    for {
      cg        <- createClassGenerator[IO](testModule, interfaces = Seq("java/util/function/Function"))
      _         <- cg.createCtor[IO](Seq.empty).use { mg =>
                     mg.addLoadThis[IO]() >> mg.addCallToObjectCtor[IO]()
                   }
      _         <- cg.createApplyMethod[IO](Seq(anyType), anyType).use { mg =>
                     mg.addLdcInsn[IO]("applied")
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz = cl.loadClass("test.pkg.TestClass")
                     val ctor  = clazz.getConstructor()
                     val obj   = ctor.newInstance()
                     val fn    = obj.asInstanceOf[java.util.function.Function[Object, Object]]
                     print(fn.apply("ignored"))
                   }
    } yield output shouldBe "applied"
  }

  it should "generate an apply method that uses its parameter" in {
    for {
      cg        <- createClassGenerator[IO](testModule, interfaces = Seq("java/util/function/Function"))
      _         <- cg.createCtor[IO](Seq.empty).use { mg =>
                     mg.addLoadThis[IO]() >> mg.addCallToObjectCtor[IO]()
                   }
      _         <- cg.createApplyMethod[IO](Seq(stringType), stringType).use { mg =>
                     mg.addLoadVar[IO](stringType, 1)
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz = cl.loadClass("test.pkg.TestClass")
                     val ctor  = clazz.getConstructor()
                     val obj   = ctor.newInstance()
                     val fn    = obj.asInstanceOf[java.util.function.Function[Object, Object]]
                     print(fn.apply("pass-through"))
                   }
    } yield output shouldBe "pass-through"
  }

  it should "generate a method that creates and returns an inner class instance" in {
    val innerModule = ModuleName(Seq("test", "pkg"), "TestClass$Box")
    val boxType     = ValueFQN(testModule, QualifiedName("Box", Qualifier.Default))

    for {
      cg        <- createClassGenerator[IO](testModule)
      innerCg   <- cg.createInnerClassGenerator[IO]("Box")
      _         <- innerCg.createField[IO]("content", stringType)
      _         <- innerCg.createCtor[IO](Seq(stringType)).use { mg =>
                     for {
                       _ <- mg.addLoadThis[IO]()
                       _ <- mg.addCallToObjectCtor[IO]()
                       _ <- mg.addLoadThis[IO]()
                       _ <- mg.addLoadVar[IO](stringType, 1)
                       _ <- mg.addPutField[IO]("content", stringType)
                     } yield ()
                   }
      _         <- cg.createMethod[IO]("makeBox", Seq(stringType), anyType).use { mg =>
                     for {
                       _ <- mg.addNew[IO](boxType)
                       _ <- mg.addLoadVar[IO](stringType, 0)
                       _ <- mg.addCallToCtor[IO](boxType, Seq(stringType))
                     } yield ()
                   }
      classFile <- cg.generate[IO]()
      innerFile <- innerCg.generate[IO]()
      output    <- runClasses(Seq(classFile, innerFile)) { cl =>
                     val clazz  = cl.loadClass("test.pkg.TestClass")
                     val method = clazz.getMethod("makeBox", classOf[String])
                     val box    = method.invoke(null, "wrapped")
                     val inner  = cl.loadClass("test.pkg.TestClass$Box")
                     val field  = inner.getField("content")
                     print(field.get(box))
                   }
    } yield output shouldBe "wrapped"
  }

  it should "generate a method that reads a field from an object parameter" in {
    val boxType = ValueFQN(testModule, QualifiedName("Box", Qualifier.Default))

    for {
      cg        <- createClassGenerator[IO](testModule)
      innerCg   <- cg.createInnerClassGenerator[IO]("Box")
      _         <- innerCg.createField[IO]("value", stringType)
      _         <- innerCg.createCtor[IO](Seq(stringType)).use { mg =>
                     for {
                       _ <- mg.addLoadThis[IO]()
                       _ <- mg.addCallToObjectCtor[IO]()
                       _ <- mg.addLoadThis[IO]()
                       _ <- mg.addLoadVar[IO](stringType, 1)
                       _ <- mg.addPutField[IO]("value", stringType)
                     } yield ()
                   }
      _         <- cg.createMethod[IO]("unbox", Seq(anyType), stringType).use { mg =>
                     for {
                       _ <- mg.addLoadVar[IO](anyType, 0)
                       _ <- mg.addCastTo[IO](boxType)
                       _ <- mg.addGetField[IO]("value", stringType, boxType)
                     } yield ()
                   }
      classFile <- cg.generate[IO]()
      innerFile <- innerCg.generate[IO]()
      output    <- runClasses(Seq(classFile, innerFile)) { cl =>
                     val innerClazz = cl.loadClass("test.pkg.TestClass$Box")
                     val ctor       = innerClazz.getConstructor(classOf[String])
                     val box        = ctor.newInstance("extracted")
                     val clazz      = cl.loadClass("test.pkg.TestClass")
                     val method     = clazz.getMethod("unbox", classOf[Object])
                     print(method.invoke(null, box))
                   }
    } yield output shouldBe "extracted"
  }

  it should "generate a method that invokes apply on a Function parameter" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createMethod[IO]("callFn", Seq(systemFunctionValue, anyType), anyType).use { mg =>
                     for {
                       _ <- mg.addLoadVar[IO](systemFunctionValue, 0)
                       _ <- mg.addLoadVar[IO](anyType, 1)
                       _ <- mg.addCallToApply[IO]()
                     } yield ()
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz                                           = cl.loadClass("test.pkg.TestClass")
                     val method                                          = clazz.getMethod("callFn", classOf[java.util.function.Function[?, ?]], classOf[Object])
                     val fn: java.util.function.Function[Object, Object] = (x: Object) => s"got $x"
                     print(method.invoke(null, fn, "input"))
                   }
    } yield output shouldBe "got input"
  }

  it should "generate a method that casts and accesses a typed value" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createMethod[IO]("castToString", Seq(anyType), stringType).use { mg =>
                     for {
                       _ <- mg.addLoadVar[IO](anyType, 0)
                       _ <- mg.addCastTo[IO](stringType)
                     } yield ()
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz  = cl.loadClass("test.pkg.TestClass")
                     val method = clazz.getMethod("castToString", classOf[Object])
                     print(method.invoke(null, "typed"))
                   }
    } yield output shouldBe "typed"
  }

  it should "generate a method that calls across modules" in {
    val otherModule = ModuleName(Seq("test", "pkg"), "Other")

    for {
      otherCg   <- createClassGenerator[IO](otherModule)
      _         <- otherCg.createMethod[IO]("provide", Seq.empty, stringType).use { mg =>
                     mg.addLdcInsn[IO]("from other")
                   }
      otherFile <- otherCg.generate[IO]()
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createMethod[IO]("fetch", Seq.empty, stringType).use { mg =>
                     val otherVfqn = ValueFQN(otherModule, QualifiedName("provide", Qualifier.Default))
                     mg.addCallTo[IO](otherVfqn, Seq.empty, stringType)
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(otherFile, classFile)) { cl =>
                     val clazz  = cl.loadClass("test.pkg.TestClass")
                     val method = clazz.getMethod("fetch")
                     print(method.invoke(null))
                   }
    } yield output shouldBe "from other"
  }

  it should "generate a module without packages" in {
    val simpleModule = ModuleName(Seq.empty, "Simple")

    for {
      cg        <- createClassGenerator[IO](simpleModule)
      _         <- cg.createMethod[IO]("hello", Seq.empty, stringType).use { mg =>
                     mg.addLdcInsn[IO]("no package")
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz  = cl.loadClass("Simple")
                     val method = clazz.getMethod("hello")
                     print(method.invoke(null))
                   }
    } yield output shouldBe "no package"
  }

  it should "generate a lambda-like inner class implementing Function" in {
    for {
      cg         <- createClassGenerator[IO](testModule)
      lambdaCg   <- cg.createInnerClassGenerator[IO]("lambda$0", interfaces = Seq("java/util/function/Function"))
      _          <- lambdaCg.createCtor[IO](Seq.empty).use { mg =>
                      mg.addLoadThis[IO]() >> mg.addCallToObjectCtor[IO]()
                    }
      _          <- lambdaCg.createApplyMethod[IO](Seq(anyType), anyType).use { mg =>
                      mg.addLdcInsn[IO]("lambda result")
                    }
      _          <- cg.createMethod[IO]("makeLambda", Seq.empty, systemFunctionValue).use { mg =>
                      val lambdaType = ValueFQN(testModule, QualifiedName("lambda$0", Qualifier.Default))
                      for {
                        _ <- mg.addNew[IO](lambdaType)
                        _ <- mg.addCallToCtor[IO](lambdaType, Seq.empty)
                      } yield ()
                    }
      classFile  <- cg.generate[IO]()
      lambdaFile <- lambdaCg.generate[IO]()
      output     <- runClasses(Seq(classFile, lambdaFile)) { cl =>
                      val clazz  = cl.loadClass("test.pkg.TestClass")
                      val method = clazz.getMethod("makeLambda")
                      val fn     = method.invoke(null).asInstanceOf[java.util.function.Function[Object, Object]]
                      print(fn.apply("anything"))
                    }
    } yield output shouldBe "lambda result"
  }

  it should "generate a closure that captures a value" in {
    for {
      cg         <- createClassGenerator[IO](testModule)
      lambdaCg   <- cg.createInnerClassGenerator[IO]("lambda$0", interfaces = Seq("java/util/function/Function"))
      _          <- lambdaCg.createField[IO]("captured", stringType)
      _          <- lambdaCg.createCtor[IO](Seq(stringType)).use { mg =>
                      for {
                        _ <- mg.addLoadThis[IO]()
                        _ <- mg.addCallToObjectCtor[IO]()
                        _ <- mg.addLoadThis[IO]()
                        _ <- mg.addLoadVar[IO](stringType, 1)
                        _ <- mg.addPutField[IO]("captured", stringType)
                      } yield ()
                    }
      lambdaType  = ValueFQN(testModule, QualifiedName("lambda$0", Qualifier.Default))
      _          <- lambdaCg.createApplyMethod[IO](Seq(anyType), anyType).use { mg =>
                      mg.addLoadThis[IO]() >>
                        mg.addGetField[IO]("captured", stringType, lambdaType)
                    }
      _          <- cg.createMethod[IO]("makeClosure", Seq(stringType), systemFunctionValue).use { mg =>
                      for {
                        _ <- mg.addNew[IO](lambdaType)
                        _ <- mg.addLoadVar[IO](stringType, 0)
                        _ <- mg.addCallToCtor[IO](lambdaType, Seq(stringType))
                      } yield ()
                    }
      classFile  <- cg.generate[IO]()
      lambdaFile <- lambdaCg.generate[IO]()
      output     <- runClasses(Seq(classFile, lambdaFile)) { cl =>
                      val clazz  = cl.loadClass("test.pkg.TestClass")
                      val method = clazz.getMethod("makeClosure", classOf[String])
                      val fn     = method.invoke(null, "closed-over").asInstanceOf[java.util.function.Function[Object, Object]]
                      print(fn.apply("ignored"))
                    }
    } yield output shouldBe "closed-over"
  }

  it should "generate an inner interface class" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      ifaceCg   <- cg.createInnerInterfaceGenerator[IO]("Show$vtable")
      classFile <- cg.generate[IO]()
      ifaceFile <- ifaceCg.generate[IO]()
      output    <- runClasses(Seq(classFile, ifaceFile)) { cl =>
                     val iface = cl.loadClass("test.pkg.TestClass$Show$vtable")
                     print(iface.isInterface.toString)
                   }
    } yield output shouldBe "true"
  }

  it should "generate an interface with an abstract method using Object types for generics" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      ifaceCg   <- cg.createInnerInterfaceGenerator[IO]("Show$vtable")
      _         <- ifaceCg.createAbstractMethod[IO]("show", Seq(anyType), anyType)
      classFile <- cg.generate[IO]()
      ifaceFile <- ifaceCg.generate[IO]()
      output    <- runClasses(Seq(classFile, ifaceFile)) { cl =>
                     val iface  = cl.loadClass("test.pkg.TestClass$Show$vtable")
                     val method = iface.getMethods.find(_.getName == "show").get
                     print(s"${method.getName}:${method.getReturnType.getSimpleName}(${method.getParameterTypes.map(_.getSimpleName).mkString})")
                   }
    } yield output shouldBe "show:Object(Object)"
  }

  it should "generate an interface with a concrete return type" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      ifaceCg   <- cg.createInnerInterfaceGenerator[IO]("Serialize$vtable")
      _         <- ifaceCg.createAbstractMethod[IO]("serialize", Seq(anyType), stringType)
      classFile <- cg.generate[IO]()
      ifaceFile <- ifaceCg.generate[IO]()
      output    <- runClasses(Seq(classFile, ifaceFile)) { cl =>
                     val iface  = cl.loadClass("test.pkg.TestClass$Serialize$vtable")
                     val method = iface.getMethods.find(_.getName == "serialize").get
                     print(s"${method.getName}:${method.getReturnType.getSimpleName}(${method.getParameterTypes.map(_.getSimpleName).mkString})")
                   }
    } yield output shouldBe "serialize:String(Object)"
  }

  it should "generate a singleton class with a static INSTANCE field" in {
    val ifaceModule = ModuleName(Seq("test", "pkg"), "TestClass")
    val implModule  = ModuleName(Seq("test", "pkg"), "ImplClass")
    val ifaceVfqn   = ValueFQN(ifaceModule, QualifiedName("Show$vtable", Qualifier.Default))

    for {
      ifaceCg   <- createClassGenerator[IO](ifaceModule)
      vtableCg  <- ifaceCg.createInnerInterfaceGenerator[IO]("Show$vtable")
      _         <- vtableCg.createAbstractMethod[IO]("show", Seq(anyType), anyType)
      ifaceFile <- ifaceCg.generate[IO]()
      vtableFile <- vtableCg.generate[IO]()
      implCg    <- createClassGenerator[IO](implModule)
      singletonCg <- implCg.createInnerClassGenerator[IO]("Show$Any$impl", Seq("test/pkg/TestClass$Show$vtable"))
      _           <- singletonCg.createStaticFinalField[IO]("INSTANCE", ifaceVfqn)
      _           <- singletonCg.createCtor[IO](Seq.empty).use { ctor =>
                       ctor.addLoadThis[IO]() >> ctor.addCallToObjectCtor[IO]()
                     }
      _           <- singletonCg.createStaticInit[IO]().use { clinit =>
                       val singletonType = ValueFQN(implModule, QualifiedName("Show$Any$impl", Qualifier.Default))
                       clinit.addNew[IO](singletonType) >>
                         clinit.addCallToCtor[IO](singletonType, Seq.empty) >>
                         clinit.addPutStaticField[IO]("INSTANCE", ifaceVfqn)
                     }
      implFile    <- implCg.generate[IO]()
      singletonFile <- singletonCg.generate[IO]()
      output    <- runClasses(Seq(ifaceFile, vtableFile, implFile, singletonFile)) { cl =>
                     val singletonClazz = cl.loadClass("test.pkg.ImplClass$Show$Any$impl")
                     val vtableClazz    = cl.loadClass("test.pkg.TestClass$Show$vtable")
                     val field          = singletonClazz.getField("INSTANCE")
                     val instance       = field.get(null)
                     print(vtableClazz.isInstance(instance).toString)
                   }
    } yield output shouldBe "true"
  }

  it should "generate a singleton that implements an interface and delegates to a static method" in {
    val ifaceModule = ModuleName(Seq("test", "pkg"), "TestClass")
    val implModule  = ModuleName(Seq("test", "pkg"), "ImplClass")
    val ifaceVfqn   = ValueFQN(ifaceModule, QualifiedName("Show$vtable", Qualifier.Default))

    for {
      ifaceCg      <- createClassGenerator[IO](ifaceModule)
      vtableCg     <- ifaceCg.createInnerInterfaceGenerator[IO]("Show$vtable")
      _            <- vtableCg.createAbstractMethod[IO]("show", Seq(anyType), anyType)
      _            <- ifaceCg.createMethod[IO]("showImpl", Seq(anyType), anyType).use { mg =>
                        mg.addLoadVar[IO](anyType, 0)
                      }
      ifaceFile    <- ifaceCg.generate[IO]()
      vtableFile   <- vtableCg.generate[IO]()
      implCg       <- createClassGenerator[IO](implModule)
      singletonCg  <- implCg.createInnerClassGenerator[IO]("Show$Any$impl", Seq("test/pkg/TestClass$Show$vtable"))
      _            <- singletonCg.createStaticFinalField[IO]("INSTANCE", ifaceVfqn)
      _            <- singletonCg.createCtor[IO](Seq.empty).use { ctor =>
                        ctor.addLoadThis[IO]() >> ctor.addCallToObjectCtor[IO]()
                      }
      _            <- singletonCg.createStaticInit[IO]().use { clinit =>
                        val singletonType = ValueFQN(implModule, QualifiedName("Show$Any$impl", Qualifier.Default))
                        clinit.addNew[IO](singletonType) >>
                          clinit.addCallToCtor[IO](singletonType, Seq.empty) >>
                          clinit.addPutStaticField[IO]("INSTANCE", ifaceVfqn)
                      }
      implVfqn      = ValueFQN(ifaceModule, QualifiedName("showImpl", Qualifier.Default))
      _            <- singletonCg.createPublicInstanceMethod[IO]("show", Seq(anyType), anyType).use { bridge =>
                        bridge.addLoadVar[IO](anyType, 1) >>
                          bridge.addCallTo[IO](implVfqn, Seq(anyType), anyType)
                      }
      implFile     <- implCg.generate[IO]()
      singletonFile <- singletonCg.generate[IO]()
      output       <- runClasses(Seq(ifaceFile, vtableFile, implFile, singletonFile)) { cl =>
                        val singletonClazz = cl.loadClass("test.pkg.ImplClass$Show$Any$impl")
                        val instanceField  = singletonClazz.getField("INSTANCE")
                        val instance       = instanceField.get(null)
                        val showMethod     = instance.getClass.getMethod("show", classOf[Object])
                        print(showMethod.invoke(instance, "delegated"))
                      }
    } yield output shouldBe "delegated"
  }

  it should "generate a method using runNative to access System.out.println" in {
    for {
      cg        <- createClassGenerator[IO](testModule)
      _         <- cg.createMainMethod[IO]().use { mg =>
                     mg.runNative[IO] { mv =>
                       mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
                       mv.visitLdcInsn("native println")
                       mv.visitMethodInsn(
                         Opcodes.INVOKEVIRTUAL,
                         "java/io/PrintStream",
                         "println",
                         "(Ljava/lang/String;)V",
                         false
                       )
                     }
                   }
      classFile <- cg.generate[IO]()
      output    <- runClasses(Seq(classFile)) { cl =>
                     val clazz  = cl.loadClass("test.pkg.TestClass")
                     val method = clazz.getMethod("main", classOf[Array[String]])
                     method.invoke(null, Array.empty[String])
                   }
    } yield output shouldBe "native println"
  }
}

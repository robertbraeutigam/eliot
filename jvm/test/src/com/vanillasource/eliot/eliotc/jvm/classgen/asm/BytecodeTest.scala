package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, PrintStream}

trait BytecodeTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  protected def runClasses(classFiles: Seq[ClassFile])(test: ClassLoader => Unit): IO[String] =
    IO.blocking {
      val classLoader = new InMemoryClassLoader(classFiles, getClass.getClassLoader)
      val baos        = new ByteArrayOutputStream()
      val printStream = new PrintStream(baos)
      val oldOut      = System.out
      try {
        System.setOut(printStream)
        Console.withOut(printStream) {
          test(classLoader)
        }
      } finally {
        System.setOut(oldOut)
      }
      printStream.flush()
      baos.toString.stripLineEnd
    }

  private class InMemoryClassLoader(classFiles: Seq[ClassFile], parent: ClassLoader)
      extends ClassLoader(parent) {
    private val classes: Map[String, Array[Byte]] = classFiles.map { cf =>
      cf.fileName.stripSuffix(".class").replace('/', '.') -> cf.bytecode
    }.toMap

    override def findClass(name: String): Class[?] =
      classes.get(name) match {
        case Some(bytes) => defineClass(name, bytes, 0, bytes.length)
        case None        => throw new ClassNotFoundException(name)
      }
  }
}

package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Unit tests for the fail-safe I/O-boundary predicate that the JVM backend enforces before emitting a native: an
  * impure leaf native (one that touches the world) must be declared `private`, so application code can never name it
  * and perform untracked I/O. A forgotten `private` is a build error, never a silent pure-typed-impure hole.
  */
class NativeImplementationTest extends AnyFlatSpec with Matchers {

  private val fqn = ValueFQN(ModuleName(Seq("eliot", "lang"), "Console"), QualifiedName("printlnInternal", Qualifier.Default))

  "the impure-native visibility check" should "reject a non-private impure native" in {
    NativeImplementation.visibilityViolation(fqn, impure = true, Visibility.Public) should not be empty
  }

  it should "accept a private impure native" in {
    NativeImplementation.visibilityViolation(fqn, impure = true, Visibility.Private) shouldBe None
  }

  it should "leave a public pure native unconstrained" in {
    NativeImplementation.visibilityViolation(fqn, impure = false, Visibility.Public) shouldBe None
  }

  "the registered must-be-private leaf natives" should "all be marked impure" in {
    // The I/O leaves (`printlnInternal`/`readLineInternal`/`logInternal`) plus the unbounded-loop divergence leaf
    // (`foreverInternal`, the `Inf` effect): each is reachable only through its public ability, so each must be private.
    val impureNames = NativeImplementation.implementations.collect {
      case (vfqn, impl) if impl.impure => vfqn.name.name
    }.toSet
    impureNames shouldBe Set("printlnInternal", "readLineInternal", "logInternal", "foreverInternal")
  }
}

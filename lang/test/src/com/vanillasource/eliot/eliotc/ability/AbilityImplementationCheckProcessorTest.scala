package com.vanillasource.eliot.eliotc.ability

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.ability.processor.{
  AbilityImplementationCheckProcessor,
  AbilityImplementationProcessor,
  ModuleAbilityOverlapCheckProcessor
}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.module.processor.{
  ModuleNamesProcessor,
  ModuleValueProcessor,
  UnifiedModuleNamesProcessor,
  UnifiedModuleValueProcessor
}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicValue}
import com.vanillasource.eliot.eliotc.monomorphize.processor.{
  DataTypeNativesProcessor,
  MonomorphicTypeCheckProcessor,
  SystemNativesProcessor,
  UserValueNativesProcessor
}
import com.vanillasource.eliot.eliotc.operator.processor.OperatorResolverProcessor
import com.vanillasource.eliot.eliotc.matchdesugar.processor.MatchDesugaringProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.token.Tokenizer

class AbilityImplementationCheckProcessorTest
    extends ProcessorTest(
      Tokenizer(),
      ASTParser(),
      CoreProcessor(),
      ModuleNamesProcessor(),
      UnifiedModuleNamesProcessor(),
      ModuleValueProcessor(Seq(ModuleName.systemFunctionModuleName)),
      UnifiedModuleValueProcessor(),
      ValueResolver(),
      MatchDesugaringProcessor(),
      OperatorResolverProcessor(),
      AbilityImplementationProcessor(),
      AbilityImplementationCheckProcessor(),
      ModuleAbilityOverlapCheckProcessor(),
      SystemNativesProcessor(),
      DataTypeNativesProcessor(),
      UserValueNativesProcessor(),
      MonomorphicTypeCheckProcessor()
    ) {
  "ability implementation check" should "pass when all methods are provided with correct signatures" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "fail when a required ability method is missing" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A\ndef display(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq("Ability implementation is missing method 'display'." at "Show"))
  }

  it should "fail when an extra method not in the ability is defined" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x\ndef extra(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq("Method not defined in ability." at "extra"))
  }

  it should "fail when an implementation method has the wrong signature" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\ndata Bool\nimplement Show[Int] { def show(x: Bool): Bool = x }\ndef f(x: Int): Int = show(x)"
    ).asserting { errors =>
      errors.length shouldBe 1
      errors.head.message should include("Signature of implementation does not match the ability definition")
      errors.head.highlight shouldBe "show"
    }
  }

  it should "pass when a non-abstract ability method is not present in the implementation" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A\ndef extra(x: A): A = x }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "resolve default ability implementation when called" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A\ndef extra(x: A): A = x }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = extra(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "resolve default ability implementation that calls another ability method" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A\ndef display(x: A): A = show(x) }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = display(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "fail when no implementations are provided at all for an ability with all-default methods" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A = x }\ndata Int\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq("The type parameter 'Int' does not implement ability 'Show'." at "Show"))
  }

  it should "resolve default ability implementation that calls another default ability implementation" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A = x\ndef display(x: A): A = show(x) }\ndata Int\nimplement Show[Int]\ndef f(a: Int): Int = display(a)"
    ).asserting(_ shouldBe Seq.empty)
  }

  // --- Ability call resolution tests (migrated from AbilityCheckProcessorTest) ---

  "ability calls" should "succeed when calling ability with generic type parameter covered by constraint" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f[A ~ Show[A]](x: A): A = show(x)",
      Seq(intType)
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "succeed when calling ability with default generic type parameter covered by constraint" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f[A ~ Show](x: A): A = show(x)",
      Seq(intType)
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "support empty abilities to be declared" in {
    runEngineForErrors(
      "ability Marker[A]\ndata Int\nimplement Marker[Int]\ndef f[A ~ Marker](x: A): A",
      Seq(intType)
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "report overlap at definition time when two impls of the same ability have unifiable patterns" in {
    // Two impls of Show both for Int → their marker signatures unify → overlap.
    // The error fires at the impls, not at the call site.
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(errors =>
      errors.count(_.message.contains("Overlapping ability implementation")) shouldBe 2
    )
  }

  it should "report overlap when one impl's pattern generalises another's" in {
    // `Show[Box[A]]` and `Show[Box[Int]]` both match `Show[Box[Int]]` → overlap at definition time.
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\ndata Box[A]\nimplement[A] Show[Box[A]] { def show(x: Box[A]): Box[A] = x }\nimplement Show[Box[Int]] { def show(x: Box[Int]): Box[Int] = x }\ndef f(x: Box[Int]): Box[Int] = show(x)"
    ).asserting(errors =>
      errors.count(_.message.contains("Overlapping ability implementation")) shouldBe 2
    )
  }

  it should "not report overlap for impls of the same ability on distinct type constructors" in {
    // `Show[Int]` and `Show[Bool]` don't overlap structurally.
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\ndata Bool\nimplement Show[Int] { def show(x: Int): Int = x }\nimplement Show[Bool] { def show(x: Bool): Bool = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(errors =>
      errors.count(_.message.contains("Overlapping ability implementation")) shouldBe 0
    )
  }

  it should "dispatch to the right impl when two impls of the same ability coexist in one module" in {
    // Two implementations of the same ability are stored under distinct per-ability indices in the
    // qualifier. Selection walks the synthetic marker function of each candidate impl to decide
    // which one matches the concrete call-site type.
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\ndata Bool\nimplement Show[Int] { def show(x: Int): Int = x }\nimplement Show[Bool] { def show(x: Bool): Bool = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "support empty ability implementations" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\nimplement Show[Int] { def show(x: Int): Int = x }\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq.empty)
  }

  it should "fail when calling ability without constraint and no implementation exists" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\ndef f[A](x: A): A = show(x)",
      Seq(intType)
    ).asserting(_ shouldBe Seq("The type parameter 'Int' does not implement ability 'Show'." at "Show"))
  }

  it should "fail when no implementation exists for the concrete type" in {
    runEngineForErrors(
      "ability Show[A] { def show(x: A): A }\ndata Int\ndef f(x: Int): Int = show(x)"
    ).asserting(_ shouldBe Seq("The type parameter 'Int' does not implement ability 'Show'." at "Show"))
  }

  it should "check derived abilities" in {
    runEngineForErrors("""
        data String
        def someString: String

        ability Show[A] {
          def show(a: A): String
        }

        implement Show[String] {
          def show(str: String): String = str
        }

        data Box[A](content: A)

        implement[A ~ Show] Show[Box[A]] {
          def show(box: Box[A]): String = show(content(box))
        }

        def f: String = show(Box(someString))
    """).asserting(_ shouldBe Seq.empty)
  }

  // --- AbilityMatcher-focused regression tests ---

  it should "dispatch multi-parameter abilities by position" in {
    // Two ability-level type params — the impl's marker pattern zips `[A, B]` with `[Int, String]` in the
    // correct order, so the dispatch only succeeds if the matcher respects declaration order.
    runEngineForErrors("""
        data String
        data Int
        def someString: String

        ability Convert[A, B] {
          def convert(x: A): B
        }

        implement Convert[Int, String] {
          def convert(x: Int): String = someString
        }

        def f(x: Int): String = convert(x)
    """).asserting(_ shouldBe Seq.empty)
  }

  it should "fail a multi-parameter ability dispatch when argument order does not match" in {
    // `Convert[String, Int]` has no impl; matching must not collapse `[Int, String]` to `[String, Int]`.
    runEngineForErrors("""
        data String
        data Int
        def someString: String

        ability Convert[A, B] {
          def convert(x: A): B
        }

        implement Convert[Int, String] {
          def convert(x: Int): String = someString
        }

        def f(x: String): Int = convert(x)
    """).asserting(errors =>
      errors.exists(_.message.contains("does not implement ability 'Convert'")) shouldBe true
    )
  }

  it should "preserve parameterised type arguments through a generic impl's binding" in {
    // A derived impl binds its type parameter to a *parameterised* type (`Pair[String, String]`). The inner
    // `show(content(box))` can only type-check if the matcher propagates the exact `Pair[String, String]`
    // GroundValue — not a `Type` fallback that would make `Show[A]` lookup fail.
    runEngineForErrors("""
        data String
        def someString: String

        ability Show[A] {
          def show(a: A): String
        }

        data Pair[F, S](first: F, second: S)
        data Box[A](content: A)

        implement Show[Pair[String, String]] {
          def show(p: Pair[String, String]): String = someString
        }

        implement[A ~ Show] Show[Box[A]] {
          def show(box: Box[A]): String = show(content(box))
        }

        def f: String = show(Box(Pair(someString, someString)))
    """).asserting(_ shouldBe Seq.empty)
  }

  it should "check signature with a generic impl that binds a type param to a parameterised type" in {
    // Signature compatibility walks the unifier with the impl marker's pattern args bound to the abstract
    // method's ability-level type param. When the pattern arg is parameterised (`Box[A]` here), the abstract
    // `show: Show[A].A -> String` must unify against the impl `show: Box[A] -> String` via structural unification.
    runEngineForErrors("""
        data String
        def someString: String

        ability Show[A] {
          def show(a: A): String
        }

        data Box[A](content: A)

        implement[A] Show[Box[A]] {
          def show(box: Box[A]): String = someString
        }

        def f(x: Box[String]): String = show(x)
    """).asserting(_ shouldBe Seq.empty)
  }

  it should "resolve an abstract associated type to the impl's concrete value" in {
    // An abstract `type X` inside an ability block is represented as a metavariable during check; the concrete
    // type from the impl (`type X = String`) is unified into that meta via the associated-type-injection pass.
    // If that pass were absent the meta would default to `Type` and the String literal argument would fail to
    // type-check against it.
    runEngineForErrors("""
        data String
        def someString: String

        ability Assoc[T] {
          type X
          def handle(v: T, p: X): String
        }

        data Name(n: String)

        implement Assoc[Name] {
          type X = String
          def handle(v: Name, p: X): String = someString
        }

        def f: String = handle(Name(someString), someString)
    """).asserting(_ shouldBe Seq.empty)
  }

  it should "chain resolutions when one ability's return type feeds another's argument" in {
    // `show(get(someName))` forces two resolution steps in the drain loop:
    //   1. `get[Name]` resolves, injecting `X = String` into the associated-type meta.
    //   2. The String now flows into `show`'s implicit type argument, letting `show[String]` resolve.
    // Plan B's drain-and-resolve loop handles the dependency chain; a single-pass resolver would leave the
    // outer `show` unresolved.
    runEngineForErrors("""
        data String
        data Name(n: String)
        def someString: String
        def someName: Name

        ability Produce[T] {
          type X
          def get(v: T): X
        }

        implement Produce[Name] {
          type X = String
          def get(v: Name): X = someString
        }

        ability Show[A] {
          def show(a: A): String
        }

        implement Show[String] {
          def show(s: String): String = s
        }

        def f: String = show(get(someName))
    """).asserting(_ shouldBe Seq.empty)
  }

  it should "reject an argument whose type contradicts the impl's associated type" in {
    // `Assoc[Name]`'s impl defines `X = String`. Calling `handle(Name(...), someInt)` supplies an Int where the
    // impl says X must be String. Without the associated-type-injection pass (or with the old flex-accept
    // escape hatch) this would silently type-check; with the pass, the meta is solved to Int by the call site
    // and then to String by the impl — they conflict, producing a type error at the call.
    runEngineForErrors("""
        data String
        data Int
        def someString: String
        def someInt: Int

        ability Assoc[T] {
          type X
          def handle(v: T, p: X): String
        }

        data Name(n: String)

        implement Assoc[Name] {
          type X = String
          def handle(v: Name, p: X): String = someString
        }

        def f: String = handle(Name(someString), someInt)
    """).asserting(errors =>
      errors.exists(e => e.message.contains("Associated type")) shouldBe true
    )
  }

  private val intType: GroundValue =
    GroundValue.Structure(
      Map(
        "$typeName" -> GroundValue.Direct(
          ValueFQN(testModuleName, QualifiedName("Int", Qualifier.Type)),
          GroundValue.Type
        )
      ),
      GroundValue.Type
    )

  private def runEngineForErrors(
      source: String,
      typeArgs: Seq[GroundValue] = Seq.empty
  ): IO[Seq[TestError]] =
    runGenerator(
      source,
      MonomorphicValue.Key(ValueFQN(testModuleName, QualifiedName("f", Qualifier.Default)), typeArgs),
      systemImports
    ).map(result => toTestErrors(result._1))
}

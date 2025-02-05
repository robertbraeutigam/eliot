package com.vanillasource.eliot.eliotc.typesystem

import cats.syntax.all.*
import cats.effect.Ref
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.resolve.TypeReference
import com.vanillasource.eliot.eliotc.resolve.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.source.CompilationIO.{CompilationIO, compilerError}

class TypeInference private (
    unifiedTypeReference: Ref[CompilationIO, TypeReference],
    scope: Ref[CompilationIO, Map[String, TypeInference]],
    equalTo: Ref[CompilationIO, Seq[TypeInference]]
)(using CompilationProcess) {
  def receivesFrom(typeReference: TypeReference): CompilationIO[TypeInference] = for {
    newTypeReference <- Ref[CompilationIO].of(typeReference)
    emptyScope       <- Ref.of[CompilationIO, Map[String, TypeInference]](Map.empty)
    newEqualTo       <- Ref.of[CompilationIO, Seq[TypeInference]](Seq(this))
    newInference      = TypeInference(newTypeReference, emptyScope, newEqualTo)
    _                <- equalTo.update(newInference +: _)
    _                <- unifyWith(typeReference)
  } yield newInference

  def inferTypeFor(typeReference: TypeReference): CompilationIO[TypeInference] = typeReference match
    case DirectTypeReference(_)     => newSameScopeInference(typeReference)
    case GenericTypeReference(name) =>
      for {
        scopeMap  <- scope.get
        inference <- scopeMap.get(name.value).map(_.pure[CompilationIO]).getOrElse(newSameScopeInference(typeReference))
        _         <- scope.set(scopeMap ++ Seq((name.value, inference)))
      } yield inference

  private def newSameScopeInference(typeReference: TypeReference) = for {
    newTypeReference <- Ref[CompilationIO].of(typeReference)
    emptyEqualTo     <- Ref.of[CompilationIO, Seq[TypeInference]](Seq.empty)
  } yield TypeInference(newTypeReference, scope, emptyEqualTo)

  private def unifyWith(incoming: TypeReference): CompilationIO[Unit] = for {
    unifiedType    <- unifiedTypeReference.get
    newUnifiedType <- unifyLocalWith(unifiedType, incoming)
    _              <- unifiedTypeReference.set(newUnifiedType)
    unifiedNodes   <- equalTo.get
    _              <- unifiedNodes.map(_.unifyWith(newUnifiedType)).sequence_.whenA(newUnifiedType != unifiedType)
  } yield ()

  private def unifyLocalWith(current: TypeReference, incoming: TypeReference): CompilationIO[TypeReference] =
    current match
      case DirectTypeReference(currentType)  =>
        incoming match
          case DirectTypeReference(incomingType) if currentType.value === incomingType.value =>
            current.pure[CompilationIO] // Same type, so return current one
          case DirectTypeReference(incomingType)                                             =>
            compilerError(
              incomingType.as(
                s"Expression with type ${incomingType.value.show} can not be assigned to type ${currentType.value.show}."
              )
            )
              .as(current)
          case GenericTypeReference(_)                                                       =>
            current.pure[CompilationIO] // Incoming more generic, so return current one
      case GenericTypeReference(genericName) => ???

}

object TypeInference {
  def forType(typeReference: TypeReference)(using CompilationProcess): CompilationIO[TypeInference] = for {
    newTypeReference <- Ref[CompilationIO].of(typeReference)
    emptyScope       <- Ref.of[CompilationIO, Map[String, TypeInference]](Map.empty)
    emptyEqualTo     <- Ref.of[CompilationIO, Seq[TypeInference]](Seq.empty)
  } yield TypeInference(newTypeReference, emptyScope, emptyEqualTo)
}

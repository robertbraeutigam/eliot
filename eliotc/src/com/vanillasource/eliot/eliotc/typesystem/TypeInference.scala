package com.vanillasource.eliot.eliotc.typesystem

import cats.syntax.all.*
import cats.effect.Ref
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.resolve.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.{GenericParameter, TypeReference}
import com.vanillasource.eliot.eliotc.resolve.TypeReference.*
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced

class TypeInference private (
    val unifiedTypeReference: Ref[CompilationIO, TypeReference],
    genericTypes: Map[String, GenericParameter],
    scope: Ref[CompilationIO, Map[String, TypeInference]],
    val equalTo: Ref[CompilationIO, Seq[TypeInference]]
)(using CompilationProcess)
    extends Logging {
  def receivesFrom(sourced: Sourced[_], inference: TypeInference): CompilationIO[TypeInference] = for {
    _                     <- equalTo.update(inference +: _)
    _                     <- inference.equalTo.update(this +: _)
    incomingTypeReference <- inference.unifiedTypeReference.get
    _                     <- unifyWith(incomingTypeReference.sourcedAt(sourced))
  } yield this

  def receivesFrom(typeReference: TypeReference): CompilationIO[TypeInference] = for {
    newTypeReference <- Ref[CompilationIO].of(typeReference)
    emptyScope       <- Ref.of[CompilationIO, Map[String, TypeInference]](Map.empty)
    newEqualTo       <- Ref.of[CompilationIO, Seq[TypeInference]](Seq(this))
    newInference      = TypeInference(newTypeReference, genericTypes, emptyScope, newEqualTo)
    _                <- typeReference match
                          case DirectTypeReference(_)     => ().pure[CompilationIO]
                          case GenericTypeReference(name) => emptyScope.update(_ ++ Seq((name.value, newInference)))
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
  } yield TypeInference(newTypeReference, genericTypes, scope, emptyEqualTo)

  private def unifyWith(incoming: TypeReference): CompilationIO[Unit] = for {
    unifiedType    <- unifiedTypeReference.get
    newUnifiedType <- unifyLocalWith(unifiedType, incoming)
    _              <- debug(s"unifying ${unifiedType.show} <- ${incoming.show} = ${newUnifiedType.show}").liftToCompilationIO
    _              <- unifiedTypeReference.set(newUnifiedType)
    unifiedNodes   <- equalTo.get
    _              <- unifiedNodes.map(_.unifyWith(newUnifiedType)).sequence_.whenA(newUnifiedType != unifiedType)
  } yield ()

  private def unifyLocalWith(current: TypeReference, incoming: TypeReference): CompilationIO[TypeReference] =
    current match
      case DirectTypeReference(currentType)                                    =>
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
      case GenericTypeReference(currentType) if isUniversal(currentType.value) =>
        incoming match
          case DirectTypeReference(incomingType)                                     =>
            compilerError(
              incomingType.as(
                s"Expression with type ${incomingType.value.show} can not be assigned to universal generic type ${currentType.value.show}."
              )
            ).as(current)
          case GenericTypeReference(incomingType) if isUniversal(incomingType.value) =>
            compilerError(
              incomingType.as(
                s"Expression with universal generic type ${incomingType.value.show} can not be assigned to universal generic type ${currentType.value.show}."
              )
            ).whenA(incomingType.value =!= currentType.value).as(current)
          case GenericTypeReference(_)                                               => current.pure[CompilationIO] // Nothing's changed, no constraints
      case GenericTypeReference(_)                                             =>
        incoming match
          case DirectTypeReference(_)  =>
            incoming.sourcedAt(current).pure[CompilationIO] // Switch to more concrete type
          case GenericTypeReference(_) => current.pure[CompilationIO] // Nothing's changed, no constraints

  private def isUniversal(genericTypeName: String) =
    genericTypes.get(genericTypeName).exists(_.isInstanceOf[UniversalGenericParameter])
}

object TypeInference {
  def forType(genericTypes: Map[String, GenericParameter], typeReference: TypeReference)(using
      CompilationProcess
  ): CompilationIO[TypeInference] = for {
    newTypeReference <- Ref[CompilationIO].of(typeReference)
    emptyScope       <- Ref.of[CompilationIO, Map[String, TypeInference]](Map.empty)
    emptyEqualTo     <- Ref.of[CompilationIO, Seq[TypeInference]](Seq.empty)
  } yield TypeInference(newTypeReference, genericTypes, emptyScope, emptyEqualTo)
}

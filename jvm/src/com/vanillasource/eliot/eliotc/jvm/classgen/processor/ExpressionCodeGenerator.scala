package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.simpleType
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.convertToNestedClassName
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.AbilityImplGenerator.{
  abilityInterfaceVfqn,
  singletonInnerName
}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}
import com.vanillasource.eliot.eliotc.uncurry.fact.*
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedExpression.*

object ExpressionCodeGenerator {

  def createExpressionCode(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      uncurriedExpression: UncurriedExpression
  ): CompilationTypesIO[Seq[ClassFile]] =
    uncurriedExpression.expression match {
      case FunctionApplication(target, arguments)   =>
        generateFunctionApplication(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          target.value,
          arguments.map(_.value),
          uncurriedExpression.expressionType
        )
      case IntegerLiteral(integerLiteral)           => ???
      case StringLiteral(stringLiteral)             =>
        methodGenerator.addLdcInsn[CompilationTypesIO](stringLiteral.value).as(Seq.empty)
      case ParameterReference(sourcedParameterName) =>
        for {
          index         <- getParameterIndex(sourcedParameterName.value)
          parameterType <- getParameterType(sourcedParameterName.value)
          _             <- compilerAbort(sourcedParameterName.as("Could not find in scope.")).liftToTypes
                             .whenA(index.isEmpty || parameterType.isEmpty)
          _             <- methodGenerator.addLoadVar[CompilationTypesIO](simpleType(parameterType.get.parameterType), index.get)
        } yield Seq.empty
      case ValueReference(sourcedVfqn)              =>
        generateFunctionApplication(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          uncurriedExpression,
          Seq.empty,
          uncurriedExpression.expressionType
        )
      case FunctionLiteral(parameters, body)        =>
        LambdaGenerator.generateLambda(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          parameters,
          body,
          createExpressionCode
        )
    }

  def computeDictParams(vfqn: ValueFQN, nameSourced: Sourced[?]): CompilerIO[Seq[ParameterDefinition]] =
    getFact(OperatorResolvedValue.Key(vfqn)).map {
      case None                => Seq.empty
      case Some(resolvedValue) =>
        resolvedValue.paramConstraints.toSeq.sortBy(_._1).flatMap { (paramName, constraints) =>
          constraints.map { constraint =>
            val interfaceVfqn = abilityInterfaceVfqn(constraint.abilityFQN)
            ParameterDefinition(
              name = nameSourced.as("$" + constraint.abilityFQN.abilityName + "$" + paramName),
              parameterType = ExpressionValue.ConcreteValue(
                Value.Structure(Map("$typeName" -> Value.Direct(interfaceVfqn, Value.Type)), Value.Type)
              )
            )
          }
        }
    }

  private def generateFunctionApplication(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      typedTarget: UncurriedExpression,
      arguments: Seq[UncurriedExpression],
      expectedResultType: ExpressionValue
  ): CompilationTypesIO[Seq[ClassFile]] =
    typedTarget.expression match {
      case IntegerLiteral(integerLiteral)          => ??? // FIXME: we can't apply functions on this, right?
      case StringLiteral(stringLiteral)            => ??? // FIXME: we can't apply functions on this, right?
      case ParameterReference(parameterName)       =>
        // Function application on a parameter reference, so this needs to be a Function
        // FIXME: This only works with 1-arguments now, since this needs to be a java.lang.Function
        for {
          parameterIndex <- getParameterIndex(parameterName.value)
          parameterType  <- getParameterType(parameterName.value)
          _              <- compilerAbort(parameterName.as("Could not find parameter in scope.")).liftToTypes
                              .whenA(parameterIndex.isEmpty || parameterType.isEmpty)
          _              <- methodGenerator
                              .addLoadVar[CompilationTypesIO](simpleType(parameterType.get.parameterType), parameterIndex.get)
          // FIXME: this does not work when currying and it fails runtime, it is not detected here
          classes        <- arguments.flatTraverse(expression =>
                              createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                            )
          _              <- methodGenerator.addCallToApply[CompilationTypesIO]()
          _              <- methodGenerator.addCastTo[CompilationTypesIO](
                              simpleType(expectedResultType)
                            )
        } yield classes
      case ValueReference(sourcedCalledVfqn)       =>
        val calledVfqn = sourcedCalledVfqn.value
        calledVfqn.name.qualifier match {
          case Qualifier.Ability(abilityName) =>
            generateAbilityMethodCall(
              moduleName,
              outerClassGenerator,
              methodGenerator,
              sourcedCalledVfqn,
              calledVfqn,
              abilityName,
              arguments,
              expectedResultType
            )
          case _                              =>
            generateNormalFunctionCall(
              moduleName,
              outerClassGenerator,
              methodGenerator,
              sourcedCalledVfqn,
              calledVfqn,
              arguments,
              expectedResultType
            )
        }
      case FunctionLiteral(parameters, body)       => ??? // FIXME: applying lambda immediately
      case FunctionApplication(target, arguments2) => ??? // FIXME: applying on a result function?
    }

  private def generateAbilityMethodCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      abilityName: String,
      arguments: Seq[UncurriedExpression],
      expectedResultType: ExpressionValue
  ): CompilationTypesIO[Seq[ClassFile]] = {
    val interfaceVfqn         = abilityInterfaceVfqn(AbilityFQN(calledVfqn.moduleName, abilityName))
    val interfaceInternalName = convertToNestedClassName(interfaceVfqn)
    for {
      uncurriedMaybe <- getFact(UncurriedValue.Key(calledVfqn, arguments.length)).liftToTypes
      resultClasses  <- uncurriedMaybe match {
                          case Some(uncurriedValue) =>
                            val parameterTypes = uncurriedValue.parameters.map(p => simpleType(p.parameterType))
                            val returnType     = simpleType(uncurriedValue.returnType)
                            for {
                              state          <- StateT.get[CompilerIO, TypeState]
                              dictParamName   = state.typeMap.keys.find(_.startsWith("$" + abilityName + "$"))
                              _              <- compilerAbort(
                                                  sourcedCalledVfqn.as(
                                                    s"Could not find dictionary parameter for ability $abilityName."
                                                  )
                                                ).liftToTypes
                                                  .whenA(dictParamName.isEmpty)
                              dictParamIndex <- getParameterIndex(dictParamName.get)
                              _              <- compilerAbort(
                                                  sourcedCalledVfqn.as(
                                                    s"Dictionary parameter ${dictParamName.get} not in scope."
                                                  )
                                                ).liftToTypes
                                                  .whenA(dictParamIndex.isEmpty)
                              _              <- methodGenerator
                                                  .addLoadVar[CompilationTypesIO](interfaceVfqn, dictParamIndex.get)
                              classes        <- arguments.flatTraverse(expression =>
                                                  createExpressionCode(
                                                    moduleName,
                                                    outerClassGenerator,
                                                    methodGenerator,
                                                    expression
                                                  )
                                                )
                              _              <- methodGenerator.addCallToAbilityMethod[CompilationTypesIO](
                                                  interfaceInternalName,
                                                  JvmIdentifier.encode(calledVfqn.name.name),
                                                  parameterTypes,
                                                  returnType
                                                )
                              _              <- methodGenerator
                                                  .addCastTo[CompilationTypesIO](simpleType(expectedResultType))
                                                  .whenA(simpleType(expectedResultType) =!= returnType)
                            } yield classes
                          case None                 =>
                            compilerError(
                              sourcedCalledVfqn.as("Could not find uncurried ability method."),
                              Seq(s"Looking for method: ${calledVfqn.show}")
                            ).liftToTypes.as(Seq.empty)
                        }
    } yield resultClasses
  }

  private def generateNormalFunctionCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      arguments: Seq[UncurriedExpression],
      expectedResultType: ExpressionValue
  ): CompilationTypesIO[Seq[ClassFile]] =
    for {
      // FIXME: calls with different currying may generate different methods here
      uncurriedMaybe     <- getFact(UncurriedValue.Key(calledVfqn, arguments.length)).liftToTypes
      resolvedValueMaybe <- getFact(OperatorResolvedValue.Key(calledVfqn)).liftToTypes
      resultClasses      <- uncurriedMaybe match
                              case Some(uncurriedValue) =>
                                val parameterTypes        = uncurriedValue.parameters.map(p => simpleType(p.parameterType))
                                val returnType            = simpleType(uncurriedValue.returnType)
                                val sortedConstraints     = resolvedValueMaybe
                                  .map(_.paramConstraints.toSeq.sortBy(_._1))
                                  .getOrElse(Seq.empty)
                                val freeTypeVarNames      = resolvedValueMaybe
                                  .map(_.paramConstraints.keys.toSet)
                                  .getOrElse(Set.empty)
                                val paramBindings         = uncurriedValue.parameters
                                  .zip(arguments)
                                  .flatMap { (pd, arg) =>
                                    ExpressionValue.matchTypes(
                                      pd.parameterType,
                                      arg.expressionType,
                                      freeTypeVarNames.contains
                                    )
                                  }
                                  .toMap
                                val sortedConstraintExprs = sortedConstraints.flatMap { (_, constraints) =>
                                  constraints.map { constraint =>
                                    val typeArgExprs = constraint.typeArgs.map {
                                      case OperatorResolvedExpression.ParameterReference(nameSrc) =>
                                        paramBindings.getOrElse(
                                          nameSrc.value,
                                          ExpressionValue.ParameterReference(nameSrc.value, Value.Type)
                                        )
                                      case _                                                      =>
                                        ExpressionValue.ParameterReference("?", Value.Type)
                                    }
                                    (constraint.abilityFQN, typeArgExprs)
                                  }
                                }
                                val dictParamTypes        = sortedConstraintExprs.map { (abilityFQN, _) =>
                                  abilityInterfaceVfqn(abilityFQN)
                                }
                                // FIXME: this doesn't seem to check whether arguments match either
                                for {
                                  _       <- sortedConstraintExprs.traverse_ { (abilityFQN, typeArgExprs) =>
                                               injectDictParam(
                                                 methodGenerator,
                                                 sourcedCalledVfqn,
                                                 abilityFQN,
                                                 typeArgExprs
                                               )
                                             }
                                  classes <-
                                    arguments.flatTraverse(expression =>
                                      createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                                    )
                                  _       <- methodGenerator.addCallTo[CompilationTypesIO](
                                               calledVfqn,
                                               dictParamTypes ++ parameterTypes,
                                               returnType
                                             )
                                  _       <- methodGenerator
                                               .addCastTo[CompilationTypesIO](
                                                 simpleType(expectedResultType)
                                               )
                                               .whenA(simpleType(expectedResultType) =!= returnType)
                                } yield classes
                              case None                 =>
                                compilerError(
                                  sourcedCalledVfqn.as("Could not find uncurried function."),
                                  Seq(s"Looking for function: ${calledVfqn.show}")
                                ).liftToTypes.as(Seq.empty)
    } yield resultClasses

  private def injectDictParam(
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      abilityFQN: AbilityFQN,
      typeArgExprs: Seq[ExpressionValue]
  ): CompilationTypesIO[Unit] = {
    val interfaceVfqn = abilityInterfaceVfqn(abilityFQN)
    val hasParamRef   = typeArgExprs.exists {
      case ExpressionValue.ParameterReference(_, _) => true
      case _                                        => false
    }
    if (hasParamRef) {
      // Generic call site: pass through caller's dict param
      val firstParamRefName = typeArgExprs
        .collectFirst { case ExpressionValue.ParameterReference(name, _) =>
          name
        }
        .getOrElse("")
      val dictParamName     = "$" + abilityFQN.abilityName + "$" + firstParamRefName
      for {
        dictParamIndex <- getParameterIndex(dictParamName)
        _              <- compilerAbort(
                            sourcedCalledVfqn.as(
                              s"Could not find dictionary parameter $dictParamName to pass through."
                            )
                          ).liftToTypes.whenA(dictParamIndex.isEmpty)
        _              <- methodGenerator.addLoadVar[CompilationTypesIO](
                            interfaceVfqn,
                            dictParamIndex.get
                          )
      } yield ()
    } else {
      // Concrete call site: inject singleton INSTANCE
      for {
        abilityModuleNames   <- getFactOrAbort(
                                  UnifiedModuleNames.Key(abilityFQN.moduleName)
                                ).liftToTypes
        abilityMethodVfqnOpt  = abilityModuleNames.names.keys.toSeq.collectFirst {
                                  case qn @ QualifiedName(_, Qualifier.Ability(aName))
                                      if aName == abilityFQN.abilityName =>
                                    ValueFQN(abilityFQN.moduleName, qn)
                                }
        _                    <- compilerAbort(
                                  sourcedCalledVfqn.as(
                                    s"Could not find any method for ability ${abilityFQN.abilityName}."
                                  )
                                ).liftToTypes.whenA(abilityMethodVfqnOpt.isEmpty)
        impl                 <- getFactOrAbort(
                                  AbilityImplementation.Key(
                                    abilityMethodVfqnOpt.get,
                                    typeArgExprs
                                  )
                                ).liftToTypes
        singletonVfqn         = ValueFQN(
                                  impl.implementationFQN.moduleName,
                                  QualifiedName(singletonInnerName(abilityFQN, typeArgExprs), Qualifier.Default)
                                )
        singletonInternalName = convertToNestedClassName(singletonVfqn)
        interfaceInternalName = convertToNestedClassName(interfaceVfqn)
        _                    <- methodGenerator.addGetStaticInstance[CompilationTypesIO](
                                  singletonInternalName,
                                  "L" + interfaceInternalName + ";"
                                )
      } yield ()
    }
  }
}

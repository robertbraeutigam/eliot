package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.simpleType
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.convertToNestedClassName
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.AbilityImplGenerator.{
  abilityInterfaceVfqn,
  singletonInnerName
}
import com.vanillasource.eliot.eliotc.symbolic.fact.{SymbolicType, TypeCheckedValue, Qualifier as SymbolicQualifier}
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
              parameterType = SymbolicType.TypeReference(interfaceVfqn)
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
      expectedResultType: SymbolicType
  ): CompilationTypesIO[Seq[ClassFile]] =
    typedTarget.expression match {
      case IntegerLiteral(integerLiteral)          => ??? // FIXME: we can't apply functions on this, right?
      case StringLiteral(stringLiteral)            => ??? // FIXME: we can't apply functions on this, right?
      case ParameterReference(parameterName)       =>
        // Function application on a parameter reference, so this needs to be a Function
        for {
          parameterIndex <- getParameterIndex(parameterName.value)
          parameterType  <- getParameterType(parameterName.value)
          _              <- compilerAbort(parameterName.as("Could not find parameter in scope.")).liftToTypes
                              .whenA(parameterIndex.isEmpty || parameterType.isEmpty)
          _              <- methodGenerator
                              .addLoadVar[CompilationTypesIO](simpleType(parameterType.get.parameterType), parameterIndex.get)
          classes        <- arguments.zipWithIndex.flatTraverse { (expression, idx) =>
                              for {
                                cs <- createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                                _  <- methodGenerator.addCallToApply[CompilationTypesIO]()
                                _  <- methodGenerator
                                        .addCastTo[CompilationTypesIO](NativeType.systemFunctionValue)
                                        .whenA(idx < arguments.size - 1)
                              } yield cs
                            }
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
          case Qualifier.AbilityImplementation(abilityName, _)
              if abilityName.value === "PatternMatch" && calledVfqn.name.name === "handleCases" =>
            generateConcreteAbilityImplCall(
              moduleName,
              outerClassGenerator,
              methodGenerator,
              sourcedCalledVfqn,
              calledVfqn,
              abilityName.value,
              arguments,
              expectedResultType
            )
          case Qualifier.AbilityImplementation(abilityName, params)
              if abilityName.value === "TypeMatch" && calledVfqn.name.name === "typeMatch" =>
            generateTypeMatchCall(
              moduleName,
              outerClassGenerator,
              methodGenerator,
              sourcedCalledVfqn,
              calledVfqn,
              params,
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
      expectedResultType: SymbolicType
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

  private def generateConcreteAbilityImplCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      abilityName: String,
      arguments: Seq[UncurriedExpression],
      expectedResultType: SymbolicType
  ): CompilationTypesIO[Seq[ClassFile]] =
    for {
      typeChecked <- getFactOrAbort(TypeCheckedValue.Key(calledVfqn)).liftToTypes
      classes     <- typeChecked.name.value.qualifier match {
                       case SymbolicQualifier.AbilityImplementation(abilityFQN, typeArgs) =>
                         generatePatternMatchCall(
                           moduleName,
                           outerClassGenerator,
                           methodGenerator,
                           sourcedCalledVfqn,
                           calledVfqn,
                           abilityFQN,
                           typeArgs,
                           arguments,
                           expectedResultType
                         )
                       case _                                                             =>
                         compilerError(
                           sourcedCalledVfqn.as("Expected AbilityImplementation qualifier for concrete ability call."),
                           Seq.empty
                         ).liftToTypes.as(Seq.empty)
                     }
    } yield classes

  private def generatePatternMatchCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      abilityFQN: AbilityFQN,
      typeArgs: Seq[SymbolicType],
      arguments: Seq[UncurriedExpression],
      expectedResultType: SymbolicType
  ): CompilationTypesIO[Seq[ClassFile]] = {
    val singletonVfqn         = ValueFQN(
      calledVfqn.moduleName,
      QualifiedName(singletonInnerName(abilityFQN, typeArgs), Qualifier.Default)
    )
    val singletonInternalName = convertToNestedClassName(singletonVfqn)
    for {
      _       <- methodGenerator.addGetStaticInstance[CompilationTypesIO](
                   singletonInternalName,
                   "L" + singletonInternalName + ";"
                 )
      classes <- arguments.flatTraverse(expression =>
                   createExpressionCode(
                     moduleName,
                     outerClassGenerator,
                     methodGenerator,
                     expression
                   )
                 )
      _       <- methodGenerator.addCallToVirtualMethod[CompilationTypesIO](
                   singletonInternalName,
                   JvmIdentifier.encode("handleCases"),
                   Seq(NativeType.systemAnyValue, NativeType.systemFunctionValue),
                   NativeType.systemAnyValue
                 )
      _       <- methodGenerator.addCastTo[CompilationTypesIO](simpleType(expectedResultType))
    } yield classes
  }

  private def generateNormalFunctionCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      arguments: Seq[UncurriedExpression],
      expectedResultType: SymbolicType
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
                                    SymbolicType.matchTypes(
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
                                          SymbolicType.TypeVariable(nameSrc.value)
                                        )
                                      case _                                                      =>
                                        SymbolicType.TypeVariable("?")
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

  private def generateTypeMatchCall(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      calledVfqn: ValueFQN,
      qualifierParams: Seq[com.vanillasource.eliot.eliotc.core.fact.Expression],
      arguments: Seq[UncurriedExpression],
      expectedResultType: SymbolicType
  ): CompilationTypesIO[Seq[ClassFile]] = {
    val constructorName = findTypeName(qualifierParams)
    for {
      _              <- compilerAbort(
                          sourcedCalledVfqn.as("Could not determine type constructor name for typeMatch.")
                        ).liftToTypes.whenA(constructorName.isEmpty)
      uncurriedMaybe <- getFact(UncurriedValue.Key(calledVfqn, arguments.length)).liftToTypes
      classes        <- uncurriedMaybe match {
                          case Some(uncurriedValue) =>
                            val parameterTypes = uncurriedValue.parameters.map(p => simpleType(p.parameterType))
                            val returnType     = simpleType(uncurriedValue.returnType)
                            for {
                              classes <- arguments.flatTraverse(expression =>
                                           createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                                         )
                              _       <- methodGenerator.addCallTo[CompilationTypesIO](
                                           calledVfqn,
                                           parameterTypes,
                                           returnType,
                                           Some("typeMatch$" + constructorName.get)
                                         )
                              _       <- methodGenerator
                                           .addCastTo[CompilationTypesIO](simpleType(expectedResultType))
                                           .whenA(simpleType(expectedResultType) =!= returnType)
                            } yield classes
                          case None                 =>
                            compilerError(
                              sourcedCalledVfqn.as("Could not find uncurried typeMatch function."),
                              Seq(s"Looking for function: ${calledVfqn.show}")
                            ).liftToTypes.as(Seq.empty)
                        }
    } yield classes
  }

  private def findTypeName(params: Seq[com.vanillasource.eliot.eliotc.core.fact.Expression]): Option[String] = {
    import com.vanillasource.eliot.eliotc.core.fact.{Expression => CoreExpression}
    def find(expr: CoreExpression): Option[String] =
      expr match {
        case CoreExpression.NamedValueReference(qn, _, _) if qn.value.qualifier == Qualifier.Type => Some(qn.value.name)
        case CoreExpression.FunctionApplication(target, _) => find(target.value)
        case _                                             => None
      }
    params.headOption.flatMap(find)
  }

  private def injectDictParam(
      methodGenerator: MethodGenerator,
      sourcedCalledVfqn: Sourced[ValueFQN],
      abilityFQN: AbilityFQN,
      typeArgExprs: Seq[SymbolicType]
  ): CompilationTypesIO[Unit] = {
    val interfaceVfqn = abilityInterfaceVfqn(abilityFQN)
    val hasParamRef   = typeArgExprs.exists {
      case SymbolicType.TypeVariable(_) => true
      case _                            => false
    }
    if (hasParamRef) {
      // Generic call site: pass through caller's dict param
      val firstParamRefName = typeArgExprs
        .collectFirst { case SymbolicType.TypeVariable(name) =>
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

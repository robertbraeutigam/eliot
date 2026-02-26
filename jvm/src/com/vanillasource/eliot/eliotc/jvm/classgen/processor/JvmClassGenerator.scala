package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.classgen.{AbilityImplGenerator, AbilityInterfaceGenerator}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.symbolic.fact.{TypeCheckedValue, Qualifier as SymbolicQualifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{addDataFieldsAndCtor, simpleType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{convertToNestedClassName, systemFunctionValue, systemUnitValue}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, MethodGenerator}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.{ClassFile, GeneratedModule}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.NativeImplementation.implementations
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}
import com.vanillasource.eliot.eliotc.uncurry.fact.*
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedExpression.*
import com.vanillasource.eliot.eliotc.used.UsedNames
import com.vanillasource.eliot.eliotc.used.UsedNames.UsageStats

import scala.annotation.tailrec

class JvmClassGenerator extends SingleKeyTypeProcessor[GeneratedModule.Key] with Logging {

  override protected def generateFact(key: GeneratedModule.Key): CompilerIO[Unit] =
    for {
      _                      <- debug[CompilerIO](s"Generating ${key.moduleName.show} (from ${key.vfqn.show}).")
      usedNames              <- getFactOrAbort(UsedNames.Key(key.vfqn))
      usedValues              = usedNames.usedNames.filter((vfqn, _) => vfqn.moduleName === key.moduleName)
      mainClassGenerator     <- createClassGenerator[CompilerIO](key.moduleName)
      // Get all module constructor names for per-data-type processing
      moduleNames            <- getFactOrAbort(UnifiedModuleNames.Key(key.moduleName))
      allCtorNames            = moduleNames.names.keys.toSeq.filter(qn => isConstructor(ValueFQN(key.moduleName, qn)))
      allCtorsWithUv         <- allCtorNames.traverseFilter { qn =>
                                  val vfqn = ValueFQN(key.moduleName, qn)
                                  getFact(UncurriedValue.Key(vfqn, 0)).map(_.map(uv => (vfqn, uv)))
                                }
      allCtorGroups           = allCtorsWithUv.groupBy((_, uv) => simpleType(constructorDataType(uv.returnType)))
      // Get used constructors with actual arity stats
      usedCtorEntries         = usedValues.filter((vfqn, _) => isConstructor(vfqn)).toSeq
      usedCtorsInfo          <- usedCtorEntries.traverseFilter { (vfqn, stats) =>
                                  getFact(UncurriedValue.Key(vfqn, stats.highestArity.getOrElse(0)))
                                    .map(_.map((vfqn, stats, _)))
                                }
      // Process each data type: check per-type eliminator usage, merge constructors, generate classes
      dataResults            <- allCtorGroups.toSeq.traverse { (typeVFQ, allTypeCtors) =>
                                  val typeName       = typeVFQ.name.name
                                  val eliminatorName = s"handle${typeName}With"
                                  val eliminatorVfqn = ValueFQN(key.moduleName, QualifiedName(eliminatorName, Qualifier.Default))
                                  val eliminatorUsed = usedValues.contains(eliminatorVfqn)
                                  val usedFromType   = usedCtorsInfo.filter(c => allTypeCtors.exists(_._1 === c._1))
                                  if (!eliminatorUsed && usedFromType.isEmpty) {
                                    (Seq.empty[ClassFile], Seq.empty[ValueFQN]).pure[CompilerIO]
                                  } else {
                                    for {
                                      eliminatorUncurried <- if (eliminatorUsed) {
                                                               val stats = usedValues(eliminatorVfqn)
                                                               getFact(UncurriedValue.Key(eliminatorVfqn, stats.highestArity.getOrElse(0)))
                                                             } else {
                                                               Option.empty[UncurriedValue].pure[CompilerIO]
                                                             }
                                      // When eliminator is used, include ALL constructors for virtual dispatch
                                      ctors                = if (eliminatorUsed) {
                                                               val usedVfqns = usedFromType.map(_._1).toSet
                                                               usedFromType ++ allTypeCtors
                                                                 .filterNot((vfqn, _) => usedVfqns.contains(vfqn))
                                                                 .map { (vfqn, uv) =>
                                                                   val stats = usedValues.getOrElse(vfqn, UsageStats(Seq.empty, Map(0 -> 1)))
                                                                   (vfqn, stats, uv)
                                                                 }
                                                             } else {
                                                               usedFromType
                                                             }
                                      classes             <- if (ctors.size === 1) {
                                                               val (vfqn, stats, _) = ctors.head
                                                               createSingleConstructorData(mainClassGenerator, vfqn, stats, eliminatorUncurried, eliminatorName)
                                                             } else {
                                                               createMultiConstructorData(mainClassGenerator, typeVFQ, ctors, eliminatorUncurried, eliminatorName)
                                                             }
                                      generatedFunctions   = {
                                                               val ctorFunctions = if (ctors.size === 1) {
                                                                 val (vfqn, _, uv) = ctors.head
                                                                 Seq(vfqn) ++ uv.parameters.map(p =>
                                                                   ValueFQN(vfqn.moduleName, QualifiedName(p.name.value, Qualifier.Default))
                                                                 )
                                                               } else {
                                                                 ctors.map(_._1)
                                                               }
                                                               ctorFunctions ++ (if (eliminatorUsed) Seq(eliminatorVfqn) else Seq.empty)
                                                             }
                                    } yield (classes, generatedFunctions)
                                  }
                                }
      dataClasses             = dataResults.flatMap(_._1)
      dataGeneratedFunctions  = dataResults.flatMap(_._2)
      abilityInterfaces      <- createAbilityInterfaces(mainClassGenerator, key.moduleName)
      abilityImpls           <- createAbilityImplSingletons(mainClassGenerator, key.moduleName)
      functionFiles          <-
        usedValues.view
          .filterKeys(k => !dataGeneratedFunctions.contains(k) && !isAbilityMethod(k))
          .toSeq
          .flatTraverse { case (vfqn, stats) =>
            // Create only non-constructor, non-accessor, non-ability-interface methods
            createModuleMethod(mainClassGenerator, vfqn, stats)
          }
      mainClass              <- mainClassGenerator.generate[CompilerIO]()
      _                      <- registerFactIfClear(
                                  GeneratedModule(
                                    key.moduleName,
                                    key.vfqn,
                                    functionFiles ++ dataClasses ++ abilityInterfaces ++ abilityImpls ++ Seq(mainClass)
                                  )
                                )
    } yield ()

  private def createModuleMethod(
      mainClassGenerator: ClassGenerator,
      vfqn: ValueFQN,
      stats: UsageStats
  ): CompilerIO[Seq[ClassFile]] = {
    implementations.get(vfqn) match {
      case Some(nativeImplementation) =>
        // There's a native implementation for this method so get it
        nativeImplementation.generateMethod(mainClassGenerator).as(Seq.empty)
      case None                       =>
        // Not a native method, should have a body and generate it
        for {
          uncurriedValue <- getFactOrAbort(UncurriedValue.Key(vfqn, stats.highestArity.getOrElse(0)))
          classFiles     <- createModuleMethod(mainClassGenerator, uncurriedValue)
          _              <- createApplicationMain(vfqn, mainClassGenerator).whenA(isMain(uncurriedValue))
        } yield classFiles
    }
  }

  private def createModuleMethod(
      classGenerator: ClassGenerator,
      uncurriedValue: UncurriedValue
  ): CompilerIO[Seq[ClassFile]] = {
    uncurriedValue.body match {
      case Some(body) =>
        for {
          dictParams <- computeDictParams(uncurriedValue.vfqn, uncurriedValue.name)
          classFiles <-
            classGenerator
              .createMethod[CompilerIO](
                uncurriedValue.vfqn.name.name,
                dictParams.map(p => simpleType(p.parameterType)) ++ uncurriedValue.parameters.map(p =>
                  simpleType(p.parameterType)
                ),
                simpleType(uncurriedValue.returnType)
              )
              .use { methodGenerator =>
                val bodyExpression = UncurriedExpression(uncurriedValue.returnType, body.value)
                val program        = for {
                  // FIXME: add parameters dynamically when we encounter them in the body!
                  _       <- dictParams.traverse_(addParameterDefinition)
                  _       <- uncurriedValue.parameters.traverse_(addParameterDefinition)
                  // Generate code for the body
                  classes <-
                    createExpressionCode(
                      uncurriedValue.vfqn.moduleName,
                      classGenerator,
                      methodGenerator,
                      bodyExpression
                    )
                  _       <-
                    debug[CompilationTypesIO](
                      s"From function ${uncurriedValue.vfqn.show}, created: ${classes.map(_.fileName).mkString(", ")}"
                    )
                } yield classes

                program.runA(TypeState(methodName = uncurriedValue.vfqn.name.name))
              }
        } yield classFiles
      case None       =>
        compilerAbort(uncurriedValue.name.as(s"Function not implemented."))
    }
  }

  private def computeDictParams(vfqn: ValueFQN, nameSourced: Sourced[?]): CompilerIO[Seq[ParameterDefinition]] =
    getFact(OperatorResolvedValue.Key(vfqn)).map {
      case None               => Seq.empty
      case Some(resolvedValue) =>
        resolvedValue.paramConstraints.toSeq.sortBy(_._1).flatMap { (paramName, constraints) =>
          constraints.map { constraint =>
            val interfaceVfqn = ValueFQN(
              constraint.abilityFQN.moduleName,
              QualifiedName(constraint.abilityFQN.abilityName + "$vtable", Qualifier.Default)
            )
            ParameterDefinition(
              name = nameSourced.as("$" + constraint.abilityFQN.abilityName + "$" + paramName),
              parameterType = ExpressionValue.ConcreteValue(
                Value.Structure(Map("$typeName" -> Value.Direct(interfaceVfqn, Value.Type)), Value.Type)
              )
            )
          }
        }
    }

  private def createExpressionCode(
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
        // This is practically a zero argument function call to this
        generateFunctionApplication(
          moduleName,
          outerClassGenerator,
          methodGenerator,
          uncurriedExpression,
          Seq.empty,
          uncurriedExpression.expressionType
        )
      case FunctionLiteral(parameters, body)        =>
        generateLambda(moduleName, outerClassGenerator, methodGenerator, parameters, body)
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
        // Calling a function
        val calledVfqn = sourcedCalledVfqn.value
        calledVfqn.name.qualifier match {
          case Qualifier.Ability(abilityName) =>
            // Ability method call - dispatch via INVOKEINTERFACE on the dictionary parameter
            val interfaceVfqn         = ValueFQN(
              calledVfqn.moduleName,
              QualifiedName(abilityName + "$vtable", Qualifier.Default)
            )
            val interfaceInternalName = convertToNestedClassName(interfaceVfqn)
            for {
              uncurriedMaybe <- getFact(UncurriedValue.Key(calledVfqn, arguments.length)).liftToTypes
              resultClasses  <- uncurriedMaybe match {
                                  case Some(uncurriedValue) =>
                                    val parameterTypes = uncurriedValue.parameters.map(p => simpleType(p.parameterType))
                                    val returnType     = simpleType(uncurriedValue.returnType)
                                    for {
                                      state         <- StateT.get[CompilerIO, TypeState]
                                      dictParamName  = state.typeMap.keys.find(_.startsWith("$" + abilityName + "$"))
                                      _             <- compilerAbort(
                                                         sourcedCalledVfqn.as(
                                                           s"Could not find dictionary parameter for ability $abilityName."
                                                         )
                                                       ).liftToTypes
                                                         .whenA(dictParamName.isEmpty)
                                      dictParamIndex <- getParameterIndex(dictParamName.get)
                                      _             <- compilerAbort(
                                                         sourcedCalledVfqn.as(
                                                           s"Dictionary parameter ${dictParamName.get} not in scope."
                                                         )
                                                       ).liftToTypes
                                                         .whenA(dictParamIndex.isEmpty)
                                      _             <- methodGenerator
                                                         .addLoadVar[CompilationTypesIO](interfaceVfqn, dictParamIndex.get)
                                      classes       <- arguments.flatTraverse(expression =>
                                                         createExpressionCode(
                                                           moduleName,
                                                           outerClassGenerator,
                                                           methodGenerator,
                                                           expression
                                                         )
                                                       )
                                      _             <- methodGenerator.addCallToAbilityMethod[CompilationTypesIO](
                                                         interfaceInternalName,
                                                         calledVfqn.name.name,
                                                         parameterTypes,
                                                         returnType
                                                       )
                                      _             <- methodGenerator
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
          case _                              =>
            // Normal function call - with dictionary injection for constrained functions
            for {
              // FIXME: calls with different currying may generate different methods here
              uncurriedMaybe     <- getFact(UncurriedValue.Key(calledVfqn, arguments.length)).liftToTypes
              resolvedValueMaybe <- getFact(OperatorResolvedValue.Key(calledVfqn)).liftToTypes
              resultClasses      <- uncurriedMaybe match
                                      case Some(uncurriedValue) =>
                                        val parameterTypes    = uncurriedValue.parameters.map(p => simpleType(p.parameterType))
                                        val returnType        = simpleType(uncurriedValue.returnType)
                                        val sortedConstraints = resolvedValueMaybe
                                                                  .map(_.paramConstraints.toSeq.sortBy(_._1))
                                                                  .getOrElse(Seq.empty)
                                        // Binding: constrained type param name → actual argument ExpressionValue
                                        val freeTypeVarNames  = resolvedValueMaybe
                                                                  .map(_.paramConstraints.keys.toSet)
                                                                  .getOrElse(Set.empty)
                                        val paramBindings     = uncurriedValue.parameters
                                                                  .zip(arguments)
                                                                  .flatMap { (pd, arg) =>
                                                                    ExpressionValue.matchTypes(
                                                                      pd.parameterType,
                                                                      arg.expressionType,
                                                                      freeTypeVarNames.contains
                                                                    )
                                                                  }
                                                                  .toMap
                                        // Map each constraint's typeArgs to concrete ExpressionValues
                                        val sortedConstraintExprs = sortedConstraints.flatMap { (_, constraints) =>
                                                                      constraints.map { constraint =>
                                                                        val typeArgExprs = constraint.typeArgs.map {
                                                                          case OperatorResolvedExpression.ParameterReference(nameSrc) =>
                                                                            paramBindings.getOrElse(
                                                                              nameSrc.value,
                                                                              ExpressionValue.ParameterReference(nameSrc.value, Value.Type)
                                                                            )
                                                                          case _ =>
                                                                            ExpressionValue.ParameterReference("?", Value.Type)
                                                                        }
                                                                        (constraint.abilityFQN, typeArgExprs)
                                                                      }
                                                                    }
                                        // Dict param types for INVOKESTATIC signature (same order as computeDictParams)
                                        val dictParamTypes = sortedConstraintExprs.map { (abilityFQN, _) =>
                                                               ValueFQN(
                                                                 abilityFQN.moduleName,
                                                                 QualifiedName(abilityFQN.abilityName + "$vtable", Qualifier.Default)
                                                               )
                                                             }
                                        // FIXME: this doesn't seem to check whether arguments match either
                                        for {
                                          // Inject dictionary arguments before regular arguments
                                          _ <- sortedConstraintExprs.traverse_ { (abilityFQN, typeArgExprs) =>
                                                 val interfaceVfqn = ValueFQN(
                                                   abilityFQN.moduleName,
                                                   QualifiedName(abilityFQN.abilityName + "$vtable", Qualifier.Default)
                                                 )
                                                 val hasParamRef   = typeArgExprs.exists {
                                                   case ExpressionValue.ParameterReference(_, _) => true
                                                   case _                                        => false
                                                 }
                                                 if (hasParamRef) {
                                                   // Generic call site: pass through caller's dict param
                                                   val firstParamRefName = typeArgExprs.collectFirst {
                                                     case ExpressionValue.ParameterReference(name, _) => name
                                                   }.getOrElse("")
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
                                                     singletonInnerName    = abilityFQN.abilityName + "$" +
                                                                               typeArgExprs.map(simpleType).map(_.name.name).mkString("$") +
                                                                               "$impl"
                                                     singletonVfqn         = ValueFQN(
                                                                               impl.implementationFQN.moduleName,
                                                                               QualifiedName(singletonInnerName, Qualifier.Default)
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
                                          // Generate regular arguments
                                          classes <- arguments.flatTraverse(expression =>
                                                       createExpressionCode(moduleName, outerClassGenerator, methodGenerator, expression)
                                                     )
                                          // Call with full signature: dict params + regular params
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
        }
      case FunctionLiteral(parameters, body)       => ??? // FIXME: applying lambda immediately
      case FunctionApplication(target, arguments2) => ??? // FIXME: applying on a result function?
    }

  private def collectParameterReferences(expr: Expression): Seq[String] =
    expr match {
      case FunctionApplication(target, arguments) =>
        collectParameterReferences(target.value.expression) ++
          arguments.flatMap(arg => collectParameterReferences(arg.value.expression))
      case FunctionLiteral(parameters, body)      =>
        collectParameterReferences(body.value.expression)
      case IntegerLiteral(_)                      => Seq.empty
      case StringLiteral(_)                       => Seq.empty
      case ParameterReference(parameterName)      => Seq(parameterName.value)
      case ValueReference(_)                      => Seq.empty
    }

  private def generateLambda(
      moduleName: ModuleName,
      outerClassGenerator: ClassGenerator,
      methodGenerator: MethodGenerator,
      parameters: Seq[ParameterDefinition],
      body: Sourced[UncurriedExpression]
  ): CompilationTypesIO[Seq[ClassFile]] = {
    // Validate parameter count - only support 0 or 1 parameter
    if (parameters.length > 1) {
      ??? // Multi-parameter lambdas not currently supported
    }

    val definition      = parameters.headOption.getOrElse(???) // Should have exactly 1 parameter
    val closedOverNames = collectParameterReferences(body.value.expression)
      .filter(_ =!= definition.name.value)
    val returnType      = simpleType(body.value.expressionType)

    for {
      closedOverArgs   <- closedOverNames.traverse(getParameterType).map(_.sequence)
      _                <- compilerAbort(body.as("Could not find all types for closed over arguments."))
                            .whenA(closedOverArgs.isEmpty)
                            .liftToTypes
      lambdaIndex      <- incLambdaCount
      methodName       <- getMethodName
      lambdaPrefix      = methodName + "$lambda$"
      lambdaFnParams    = closedOverArgs.get ++ parameters
      // Save outer state and set up a fresh TypeState for the lambda body.
      // The lambdaFn static method only has lambdaFnParams as its JVM parameters,
      // so parameter indices must be relative to that, not the outer scope.
      outerState       <- StateT.get[CompilerIO, TypeState]
      _                <- StateT.set[CompilerIO, TypeState](TypeState(
                            typeMap = lambdaFnParams.map(p => p.name.value -> p).toMap,
                            parameters = lambdaFnParams.map(_.name.value),
                            lambdaCount = outerState.lambdaCount,
                            methodName = methodName
                          ))
      cls1             <-
        outerClassGenerator
          .createMethod[CompilationTypesIO](
            lambdaPrefix + "fn$" + lambdaIndex,
            lambdaFnParams.map(_.parameterType).map(simpleType),
            simpleType(body.value.expressionType)
          )
          .use { fnGenerator =>
            createExpressionCode(moduleName, outerClassGenerator, fnGenerator, body.value)
          }
      // Restore outer state, preserving lambdaCount from inner (nested lambdas may have incremented it)
      innerState       <- StateT.get[CompilerIO, TypeState]
      _                <- StateT.set[CompilerIO, TypeState](outerState.copy(lambdaCount = innerState.lambdaCount))
      innerClassWriter <-
        outerClassGenerator
          .createInnerClassGenerator[CompilationTypesIO](lambdaPrefix + lambdaIndex, Seq("java/util/function/Function"))
      _                <- innerClassWriter.addDataFieldsAndCtor[CompilationTypesIO](closedOverArgs.get)
      _                <- innerClassWriter
                            .createApplyMethod[CompilationTypesIO](
                              Seq(simpleType(definition.parameterType)),
                              simpleType(body.value.expressionType)
                            )
                            .use { applyGenerator =>
                              for {
                                _ <- closedOverArgs.get.traverse_ { argument =>
                                       for {
                                         _ <- applyGenerator
                                                .addLoadVar[CompilationTypesIO](
                                                  ValueFQN(moduleName, QualifiedName(lambdaPrefix + lambdaIndex, Qualifier.Default)),
                                                  0 // The data object is the parameter
                                                )
                                         _ <- applyGenerator.addGetField[CompilationTypesIO](
                                                argument.name.value,
                                                simpleType(argument.parameterType),
                                                ValueFQN(moduleName, QualifiedName(lambdaPrefix + lambdaIndex, Qualifier.Default))
                                              )
                                       } yield ()
                                     }
                                // Load the lambda parameter from apply method argument (0=this, 1=arg)
                                _ <- applyGenerator.addLoadVar[CompilationTypesIO](
                                       simpleType(definition.parameterType),
                                       1
                                     )
                                _ <- applyGenerator.addCastTo[CompilationTypesIO](
                                       simpleType(definition.parameterType)
                                     )
                                // Call the static lambdaFn
                                _ <- applyGenerator.addCallTo[CompilationTypesIO](
                                       ValueFQN(moduleName, QualifiedName(lambdaPrefix + "fn$" + lambdaIndex, Qualifier.Default)),
                                       lambdaFnParams.map(_.parameterType).map(simpleType),
                                       simpleType(body.value.expressionType)
                                     )
                              } yield ()
                            }
      classFile        <- innerClassWriter.generate[CompilationTypesIO]()
      _                <- methodGenerator.addNew[CompilationTypesIO](ValueFQN(moduleName, QualifiedName(lambdaPrefix + lambdaIndex, Qualifier.Default)))
      _                <- closedOverArgs.get.traverse_ { argument =>
                            for {
                              argIndex <- getParameterIndex(argument.name.value)
                              argType  <- getParameterType(argument.name.value)
                              _        <- methodGenerator
                                            .addLoadVar[CompilationTypesIO](simpleType(argType.get.parameterType), argIndex.get)
                            } yield ()
                          }
      _                <- methodGenerator.addCallToCtor[CompilationTypesIO]( // Call constructor
                            ValueFQN(moduleName, QualifiedName(lambdaPrefix + lambdaIndex, Qualifier.Default)),
                            closedOverArgs.get.map(_.parameterType).map(simpleType)
                          )
      // FIXME: add apply: calling the static method
    } yield classFile +: cls1
  }

  /** Create a JVM compatible static main, if this method is the eliot main, presumably generated from the
    * JvmProgramGenerator.
    */
  private def createApplicationMain(mainVfqn: ValueFQN, generator: ClassGenerator): CompilerIO[Unit] =
    generator.createMainMethod[CompilerIO]().use { methodGenerator =>
      methodGenerator.addCallTo(mainVfqn, Seq.empty, systemUnitValue)
    }

  /** @return
    *   Iff the method definition is a suitable main to run from the JVM
    */
  private def isMain(uncurriedValue: UncurriedValue): Boolean =
    uncurriedValue.name.value.name === "main" && uncurriedValue.parameters.isEmpty

  /** Extract the actual data type from a constructor's return type by stripping function type wrappers.
    * Nullary constructors return the data type directly (e.g. `Maybe[String]`), while constructors with
    * fields return a function type (e.g. `String -> Maybe[String]`). This strips the function types
    * to get the underlying data type in both cases.
    */
  @tailrec
  private def constructorDataType(returnType: ExpressionValue): ExpressionValue =
    returnType match {
      case ExpressionValue.FunctionType(_, inner) => constructorDataType(inner)
      case other                                  => other
    }

  /** Single-constructor data: generates a concrete class, factory method, accessors, and optional eliminator. */
  private def createSingleConstructorData(
      outerClassGenerator: ClassGenerator,
      valueFQN: ValueFQN,
      stats: UsageStats,
      handleWithUncurried: Option[UncurriedValue],
      eliminatorName: String
  ): CompilerIO[Seq[ClassFile]] =
    for {
      uncurriedValueMaybe <- getFact(UncurriedValue.Key(valueFQN, stats.highestArity.getOrElse(0)))
      classes             <- uncurriedValueMaybe match {
                               case Some(uncurriedValue) =>
                                 for {
                                   _  <- debug[CompilerIO](s"Creating data type from constructor '${valueFQN.show}'")
                                   cs <- createDataClassWithHandleWith(
                                           outerClassGenerator,
                                           valueFQN.name.name,
                                           uncurriedValue.parameters,
                                           Seq.empty,
                                           handleWithUncurried.map(hw => Seq((0, uncurriedValue.parameters, hw))),
                                           eliminatorName
                                         )
                                   // Define data function
                                   _  <-
                                     outerClassGenerator
                                       .createMethod[CompilerIO](
                                         valueFQN.name.name,
                                         uncurriedValue.parameters.map(_.parameterType).map(simpleType),
                                         valueFQN
                                       )
                                       .use { methodGenerator =>
                                         for {
                                           _ <- methodGenerator.addNew[CompilerIO](valueFQN)
                                           _ <- uncurriedValue.parameters.zipWithIndex.traverse_ { (fieldDefinition, index) =>
                                                  methodGenerator
                                                    .addLoadVar[CompilerIO](simpleType(fieldDefinition.parameterType), index)
                                                }
                                           _ <- methodGenerator.addCallToCtor[CompilerIO](
                                                  valueFQN,
                                                  uncurriedValue.parameters
                                                    .map(_.parameterType)
                                                    .map(simpleType)
                                                )
                                         } yield ()
                                       }
                                   // Define accessors
                                   _  <- uncurriedValue.parameters.traverse_ { argumentDefinition =>
                                           outerClassGenerator
                                             .createMethod[CompilerIO](
                                               argumentDefinition.name.value,
                                               Seq(valueFQN),
                                               simpleType(argumentDefinition.parameterType)
                                             )
                                             .use { accessorGenerator =>
                                               for {
                                                 _ <- accessorGenerator
                                                        .addLoadVar[CompilerIO](
                                                          valueFQN,
                                                          0
                                                        )
                                                 _ <- accessorGenerator.addGetField[CompilerIO](
                                                        argumentDefinition.name.value,
                                                        simpleType(argumentDefinition.parameterType),
                                                        valueFQN
                                                      )
                                               } yield ()
                                             }
                                         }
                                   // Generate static eliminator method (delegates to virtual call on the class)
                                   _  <- handleWithUncurried.traverse_ { hw =>
                                           generateStaticHandleWith(outerClassGenerator, hw, valueFQN, isInterface = false, eliminatorName)
                                         }
                                 } yield cs
                               case None                 =>
                                 error[CompilerIO](s"Could not resolve '${valueFQN.show}'.") >> Seq.empty.pure[CompilerIO]
                             }
    } yield classes

  /** Multi-constructor (union) data: generates an interface + implementation classes + factory methods + eliminator. */
  private def createMultiConstructorData(
      outerClassGenerator: ClassGenerator,
      typeVFQ: ValueFQN,
      ctors: Seq[(ValueFQN, UsageStats, UncurriedValue)],
      handleWithUncurried: Option[UncurriedValue],
      eliminatorName: String
  ): CompilerIO[Seq[ClassFile]] =
    for {
      _              <- debug[CompilerIO](s"Creating union data type '${typeVFQ.show}' with ${ctors.size} constructors")
      // Create the interface for the data type
      interfaceGen   <- outerClassGenerator.createInnerInterfaceGenerator[CompilerIO](typeVFQ.name.name)
      // Add abstract eliminator method to the interface if used
      _              <- handleWithUncurried.traverse_ { hw =>
                          val handlerParams = hw.parameters.drop(1) // skip 'obj' parameter
                          interfaceGen.createAbstractMethod[CompilerIO](
                            eliminatorName,
                            handlerParams.map(_.parameterType).map(simpleType),
                            simpleType(hw.returnType)
                          )
                        }
      interfaceClass <- interfaceGen.generate[CompilerIO]()
      interfaceName   = convertToNestedClassName(typeVFQ)
      // Create implementation classes and factory methods for each constructor
      // Sort constructors by source position to match definition order (= handleWith handler order)
      sortedCtors     = ctors.sortBy { case (_, _, uv) => (uv.name.range.from.line, uv.name.range.from.col) }
      ctorIndexMap     = sortedCtors.zipWithIndex.map { case ((vfqn, _, _), idx) => vfqn -> idx }.toMap
      implClasses    <- ctors.flatTraverse { case (vfqn, _, uncurriedValue) =>
                          val ctorIndex = ctorIndexMap.getOrElse(vfqn, 0)
                          for {
                            _  <- debug[CompilerIO](s"Creating union constructor '${vfqn.show}' implementing '${typeVFQ.show}'")
                            cs <- createDataClassWithHandleWith(
                                    outerClassGenerator,
                                    vfqn.name.name,
                                    uncurriedValue.parameters,
                                    Seq(interfaceName),
                                    handleWithUncurried.map(hw => Seq((ctorIndex, uncurriedValue.parameters, hw))),
                                    eliminatorName
                                  )
                            // Factory method returns the interface type
                            _  <-
                              outerClassGenerator
                                .createMethod[CompilerIO](
                                  vfqn.name.name,
                                  uncurriedValue.parameters.map(_.parameterType).map(simpleType),
                                  typeVFQ
                                )
                                .use { methodGenerator =>
                                  for {
                                    _ <- methodGenerator.addNew[CompilerIO](vfqn)
                                    _ <- uncurriedValue.parameters.zipWithIndex.traverse_ { (fieldDef, index) =>
                                           methodGenerator
                                             .addLoadVar[CompilerIO](simpleType(fieldDef.parameterType), index)
                                         }
                                    _ <- methodGenerator.addCallToCtor[CompilerIO](
                                           vfqn,
                                           uncurriedValue.parameters.map(_.parameterType).map(simpleType)
                                         )
                                  } yield ()
                                }
                          } yield cs
                        }
      // Generate static eliminator method (delegates to INVOKEINTERFACE)
      _              <- handleWithUncurried.traverse_ { hw =>
                          generateStaticHandleWith(outerClassGenerator, hw, typeVFQ, isInterface = true, eliminatorName)
                        }
    } yield Seq(interfaceClass) ++ implClasses

  /** Create a data class with fields, constructor, and optionally an eliminator instance method.
    * @param handleWithInfo
    *   If present, Seq of (constructorIndex, constructorFields, handleWithUncurried) for generating eliminator override.
    *   For single-constructor data, this is Seq((0, fields, hw)). For union constructors, each gets its own index.
    */
  private def createDataClassWithHandleWith(
      outerClassGenerator: ClassGenerator,
      innerClassName: String,
      fields: Seq[ParameterDefinition],
      javaInterfaces: Seq[String] = Seq.empty,
      handleWithInfo: Option[Seq[(Int, Seq[ParameterDefinition], UncurriedValue)]] = None,
      eliminatorName: String = ""
  ): CompilerIO[Seq[ClassFile]] =
    for {
      innerClassWriter <- outerClassGenerator.createInnerClassGenerator[CompilerIO](innerClassName, javaInterfaces)
      _                <- innerClassWriter.addDataFieldsAndCtor[CompilerIO](fields)
      // Generate eliminator instance method override if requested
      _                <- handleWithInfo.traverse_ { infos =>
                            infos.traverse_ { case (ctorIndex, ctorFields, hw) =>
                              val handlerParams = hw.parameters.drop(1) // skip 'obj' parameter
                              innerClassWriter
                                .createPublicInstanceMethod[CompilerIO](
                                  eliminatorName,
                                  handlerParams.map(_.parameterType).map(simpleType),
                                  simpleType(hw.returnType)
                                )
                                .use { methodGenerator =>
                                  // Load this constructor's handler (at param index ctorIndex + 1, since 0 is 'this')
                                  val handlerLocalIndex = ctorIndex + 1
                                  for {
                                    _ <- methodGenerator.addLoadVar[CompilerIO](systemFunctionValue, handlerLocalIndex)
                                    // Apply handler to fields (or unit for zero fields)
                                    _ <-
                                      if (ctorFields.isEmpty) {
                                        // Zero fields: handler is Function[Unit, R], call apply(null)
                                        for {
                                          _ <- methodGenerator.addConstNull[CompilerIO]()
                                          _ <- methodGenerator.addCallToApply[CompilerIO]()
                                        } yield ()
                                      } else {
                                        // One or more fields: curried application handler(field1)(field2)...
                                        ctorFields.zipWithIndex.traverse_ { (fieldDef, fieldIndex) =>
                                          val vfqn = ValueFQN(
                                            outerClassGenerator.moduleName,
                                            QualifiedName(innerClassName, Qualifier.Default)
                                          )
                                          for {
                                            _ <- methodGenerator.addLoadThis[CompilerIO]()
                                            _ <- methodGenerator.addGetField[CompilerIO](
                                                   fieldDef.name.value,
                                                   simpleType(fieldDef.parameterType),
                                                   vfqn
                                                 )
                                            _ <- methodGenerator.addCallToApply[CompilerIO]()
                                            // Cast intermediate results to Function for subsequent curried calls
                                            _ <- methodGenerator
                                                   .addCastTo[CompilerIO](systemFunctionValue)
                                                   .whenA(fieldIndex < ctorFields.size - 1)
                                          } yield ()
                                        }
                                      }
                                  } yield ()
                                }
                            }
                          }
      classFile        <- innerClassWriter.generate[CompilerIO]()
    } yield Seq(classFile)

  /** Generate a static eliminator method on the module class that delegates to virtual/interface dispatch. */
  private def generateStaticHandleWith(
      outerClassGenerator: ClassGenerator,
      hw: UncurriedValue,
      dataTypeVfqn: ValueFQN,
      isInterface: Boolean,
      eliminatorName: String
  ): CompilerIO[Unit] = {
    val allParamTypes = hw.parameters.map(_.parameterType).map(simpleType)
    val returnType    = simpleType(hw.returnType)
    val handlerParams = hw.parameters.drop(1) // skip 'obj'
    outerClassGenerator
      .createMethod[CompilerIO](
        eliminatorName,
        allParamTypes,
        returnType
      )
      .use { methodGenerator =>
        for {
          // Load obj (static method param 0)
          _ <- methodGenerator.addLoadVar[CompilerIO](simpleType(hw.parameters.head.parameterType), 0)
          // Load all handler params (starting at 1)
          _ <- handlerParams.zipWithIndex.traverse_ { (param, index) =>
                 methodGenerator.addLoadVar[CompilerIO](simpleType(param.parameterType), index + 1)
               }
          // Dispatch: INVOKEINTERFACE for union types, INVOKEVIRTUAL for single-constructor
          _ <-
            if (isInterface) {
              methodGenerator.addCallToAbilityMethod[CompilerIO](
                convertToNestedClassName(dataTypeVfqn),
                eliminatorName,
                handlerParams.map(_.parameterType).map(simpleType),
                returnType
              )
            } else {
              methodGenerator.addCallToVirtualMethod[CompilerIO](
                convertToNestedClassName(dataTypeVfqn),
                eliminatorName,
                handlerParams.map(_.parameterType).map(simpleType),
                returnType
              )
            }
        } yield ()
      }
  }



  private def createAbilityInterfaces(
      mainClassGenerator: ClassGenerator,
      moduleName: ModuleName
  ): CompilerIO[Seq[ClassFile]] =
    for {
      unifiedModuleNames <- getFactOrAbort(UnifiedModuleNames.Key(moduleName))
      abilityMethodsByAbility = unifiedModuleNames.names.keys.toSeq.collect {
                                  case qn @ QualifiedName(_, Qualifier.Ability(abilityName)) =>
                                    (abilityName, ValueFQN(moduleName, qn))
                                }.groupMap(_._1)(_._2)
      classFiles         <- abilityMethodsByAbility.toSeq.flatTraverse { (abilityName, methodVfqns) =>
                              createAbilityInterface(mainClassGenerator, abilityName, methodVfqns)
                            }
    } yield classFiles

  private def createAbilityInterface(
      outerClassGenerator: ClassGenerator,
      abilityName: String,
      methodVfqns: Seq[ValueFQN]
  ): CompilerIO[Seq[ClassFile]] =
    for {
      methods   <- methodVfqns.traverse(vfqn => getFactOrAbort(UncurriedValue.Key(vfqn, 0)))
      classFile <- AbilityInterfaceGenerator.createAbilityInterface[CompilerIO](outerClassGenerator, abilityName, methods)
    } yield Seq(classFile)

  private def createAbilityImplSingletons(
      mainClassGenerator: ClassGenerator,
      moduleName: ModuleName
  ): CompilerIO[Seq[ClassFile]] =
    for {
      unifiedModuleNames <- getFactOrAbort(UnifiedModuleNames.Key(moduleName))
      implNames           = unifiedModuleNames.names.keys.toSeq.collect {
                              case qn @ QualifiedName(_, Qualifier.AbilityImplementation(_, _)) =>
                                ValueFQN(moduleName, qn)
                            }
      implsWithAbility   <- implNames.flatTraverse { implVfqn =>
                              getFactOrAbort(TypeCheckedValue.Key(implVfqn)).map { checked =>
                                checked.name.value.qualifier match {
                                  case SymbolicQualifier.AbilityImplementation(abilityFQN, typeArgs) =>
                                    Seq((implVfqn, abilityFQN, typeArgs))
                                  case _                                                             =>
                                    Seq.empty
                                }
                              }
                            }
      grouped             = implsWithAbility
                              .groupMap { case (_, abilityFQN, typeArgs) => (abilityFQN, typeArgs) } {
                                case (vfqn, _, _) => vfqn
                              }
      classFiles         <- grouped.toSeq.flatTraverse { case ((abilityFQN, typeArgs), implVfqns) =>
                              createAbilityImplSingleton(mainClassGenerator, abilityFQN, typeArgs, implVfqns)
                            }
    } yield classFiles

  private def createAbilityImplSingleton(
      mainClassGenerator: ClassGenerator,
      abilityFQN: AbilityFQN,
      typeArgs: Seq[ExpressionValue],
      implVfqns: Seq[ValueFQN]
  ): CompilerIO[Seq[ClassFile]] =
    for {
      methodPairs <- implVfqns.traverse { implVfqn =>
                       val abilityMethodVfqn = ValueFQN(
                         abilityFQN.moduleName,
                         QualifiedName(implVfqn.name.name, Qualifier.Ability(abilityFQN.abilityName))
                       )
                       for {
                         abilityMethod <- getFactOrAbort(UncurriedValue.Key(abilityMethodVfqn, 0))
                         implMethod    <- getFactOrAbort(UncurriedValue.Key(implVfqn, 0))
                       } yield (abilityMethod, implMethod)
                     }
      classFile   <- AbilityImplGenerator.createAbilityImpl[CompilerIO](
                       mainClassGenerator,
                       abilityFQN,
                       typeArgs,
                       methodPairs
                     )
    } yield Seq(classFile)

  private def isAbilityMethod(valueFQN: ValueFQN): Boolean =
    valueFQN.name.qualifier match {
      case Qualifier.Ability(_) => true
      case _                    => false
    }

  private def isConstructor(valueFQN: ValueFQN): Boolean =
    valueFQN.name.qualifier === Qualifier.Default && valueFQN.name.name.charAt(0).isUpper
}

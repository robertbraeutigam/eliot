package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{
  ArgumentDefinition,
  DataConstructor,
  DataDefinition,
  FunctionDefinition,
  GenericParameter,
  Visibility,
  Expression as SourceExpression,
  QualifiedName as AstQualifiedName,
  Qualifier as AstQualifier,
  Pattern as SourcePattern
}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Desugars data definitions into synthetic FunctionDefinitions. Works entirely in the source AST domain.
  *
  * Generates:
  *   - A type function that returns the kind of the type
  *   - Constructor functions for each constructor
  *   - Accessor functions for each field (only if there is exactly one constructor)
  *   - An eliminator function "handleWith" for pattern-matching dispatch
  */
object DataDefinitionDesugarer {

  def desugar(definition: DataDefinition): Seq[FunctionDefinition] = {
    val mainFunctions =
      (createTypeFunction(definition) ++ createConstructors(definition) ++ createAccessors(definition))
        .map(_.copy(visibility = definition.visibility))
    mainFunctions ++ createEliminator(definition) ++ createTypeMatch(definition)
  }

  private def createTypeFunction(definition: DataDefinition): Seq[FunctionDefinition] =
    Seq(
      FunctionDefinition(
        definition.name.map(n => AstQualifiedName(n, AstQualifier.Type)),
        Seq.empty,
        definition.genericParameters.map(gp => ArgumentDefinition(gp.name, gp.typeRestriction)),
        typeExpr(definition.name.as("Type")),
        None
      )
    )

  private def createConstructors(definition: DataDefinition): Seq[FunctionDefinition] =
    definition.constructors
      .getOrElse(Seq.empty)
      .map(ctor => createConstructor(definition, ctor))

  private def createConstructor(definition: DataDefinition, ctor: DataConstructor): FunctionDefinition =
    FunctionDefinition(
      ctor.name.map(n => AstQualifiedName(n, AstQualifier.Default)),
      definition.genericParameters,
      ctor.fields,
      typeExpr(
        definition.name,
        definition.genericParameters.map(gp => typeExpr(gp.name))
      ),
      None
    )

  /** Accessors are only created when there is exactly one constructor. Each accessor is implemented as a match
    * expression that deconstructs the object and returns the target field.
    */
  private def createAccessors(definition: DataDefinition): Seq[FunctionDefinition] =
    definition.constructors
      .filter(_.size === 1)
      .getOrElse(Seq.empty)
      .flatMap(ctor => ctor.fields.map(field => createAccessor(definition, ctor, field)))

  private def createAccessor(
      definition: DataDefinition,
      ctor: DataConstructor,
      field: ArgumentDefinition
  ): FunctionDefinition = {
    val matchBody = SourceExpression.MatchExpression(
      field.name.as(SourceExpression.FunctionApplication(None, field.name.as("obj"), Seq.empty, Seq.empty)),
      Seq(
        SourceExpression.MatchCase(
          field.name.as(SourcePattern.ConstructorPattern(
            None,
            ctor.name,
            ctor.fields.map { f =>
              if (f.name.value == field.name.value)
                f.name.as(SourcePattern.VariablePattern(field.name))
              else
                f.name.as(SourcePattern.WildcardPattern(f.name.as("_")))
            }
          )),
          field.name.as(SourceExpression.FunctionApplication(None, field.name, Seq.empty, Seq.empty))
        )
      )
    )
    FunctionDefinition(
      field.name.map(n => AstQualifiedName(n, AstQualifier.Default)),
      definition.genericParameters,
      Seq(
        ArgumentDefinition(
          field.name.as("obj"),
          typeExpr(
            definition.name,
            definition.genericParameters.map(gp => typeExpr(gp.name))
          )
        )
      ),
      field.typeExpression,
      Some(field.name.as(matchBody))
    )
  }

  /** Create an eliminator function "handle<TypeName>With" for data types with constructors. The eliminator takes the
    * data object and one handler function per constructor. Each handler takes the constructor's fields (curried) and
    * returns a result of type R. The eliminator always has public visibility to avoid name collisions.
    *
    * Example: data Option[A] = None | Some(a: A) generates: handleOptionWith[A, R](obj: Option[A], noneCase:
    * Function[Unit, R], someCase: Function[A, R]): R
    */
  private def createEliminator(definition: DataDefinition): Seq[FunctionDefinition] =
    definition.constructors
      .filter(_.nonEmpty)
      .toSeq
      .map { ctors =>
        val existingNames    = definition.genericParameters.map(_.name.value).toSet
        val resultParamName  = freshName("R", existingNames)
        val resultParam      = GenericParameter(
          definition.name.as(resultParamName),
          typeExpr(definition.name.as("Type")),
          Seq.empty
        )
        val allGenericParams = definition.genericParameters :+ resultParam
        val dataTypeRef = typeExpr(
          definition.name,
          definition.genericParameters.map(gp => typeExpr(gp.name))
        )
        val objArg = ArgumentDefinition(
          definition.name.as("obj"),
          dataTypeRef
        )
        val handlerArgs = ctors.map { ctor =>
          val handlerName = definition.name.as((ctor.name.value.head.toLower +: ctor.name.value.tail) + "Case")
          val handlerType = eliminatorHandlerType(definition, ctor, resultParamName)
          ArgumentDefinition(handlerName, handlerType)
        }
        val allArgs    = Seq(objArg) ++ handlerArgs
        val returnType = typeExpr(definition.name.as(resultParamName))
        FunctionDefinition(
          definition.name.map(_ => AstQualifiedName(s"handle${definition.name.value}With", AstQualifier.Default)),
          allGenericParams,
          allArgs,
          returnType,
          None,
          visibility = Visibility.Public
        )
      }

  /** Create a type match discriminator "typeMatch<TypeName>" for each data definition. This is a binary discriminator
    * that checks if a Type value was constructed by this data type's type constructor. It takes the Type object, a match
    * handler (curried over the generic parameters), and an else handler for when the type doesn't match.
    *
    * Example: data Person[NAME: String](content: String) generates: typeMatchPerson[R](obj: Type, personCase:
    * Function[String, R], elseCase: Function[Unit, R]): R
    */
  private def createTypeMatch(definition: DataDefinition): Seq[FunctionDefinition] = {
    val existingNames   = definition.genericParameters.map(_.name.value).toSet
    val resultParamName = freshName("R", existingNames)
    val resultParam     = GenericParameter(
      definition.name.as(resultParamName),
      typeExpr(definition.name.as("Type")),
      Seq.empty
    )
    val allGenericParams = definition.genericParameters :+ resultParam
    val objArg           = ArgumentDefinition(
      definition.name.as("obj"),
      typeExpr(definition.name.as("Type"))
    )
    val matchCaseType = typeMatchHandlerType(definition, resultParamName)
    val matchCaseArg  = ArgumentDefinition(definition.name.as("matchCase"), matchCaseType)
    val elseCaseType  = typeExpr(
      definition.name.as("Function"),
      Seq(
        typeExpr(definition.name.as("Unit")),
        typeExpr(definition.name.as(resultParamName))
      )
    )
    val elseCaseArg = ArgumentDefinition(definition.name.as("elseCase"), elseCaseType)
    val returnType  = typeExpr(definition.name.as(resultParamName))
    Seq(
      FunctionDefinition(
        definition.name.map(_ => AstQualifiedName(s"typeMatch${definition.name.value}", AstQualifier.Default)),
        allGenericParams,
        Seq(objArg, matchCaseArg, elseCaseArg),
        returnType,
        None,
        visibility = Visibility.Public
      )
    )
  }

  /** Build the match case handler type for a type match. Zero generic params: Function[Unit, R]. Otherwise: curried
    * Function[GP1.typeRestriction, Function[GP2.typeRestriction, ..., R]].
    */
  private def typeMatchHandlerType(definition: DataDefinition, resultParamName: String): Sourced[SourceExpression] =
    if (definition.genericParameters.isEmpty) {
      typeExpr(
        definition.name.as("Function"),
        Seq(
          typeExpr(definition.name.as("Unit")),
          typeExpr(definition.name.as(resultParamName))
        )
      )
    } else {
      definition.genericParameters.foldRight(typeExpr(definition.name.as(resultParamName))) { (gp, acc) =>
        typeExpr(definition.name.as("Function"), Seq(gp.typeRestriction, acc))
      }
    }

  private def freshName(base: String, existingNames: Set[String]): String =
    if (!existingNames.contains(base)) base
    else Iterator.from(0).map(i => s"$base$i").find(!existingNames.contains(_)).get

  /** Build the handler type for an eliminator. Zero fields: Function[Unit, R]. One field: Function[FieldType, R].
    * Multiple fields: curried Function[Field1, Function[Field2, ..., R]].
    */
  private def eliminatorHandlerType(
      definition: DataDefinition,
      ctor: DataConstructor,
      resultParamName: String
  ): Sourced[SourceExpression] =
    if (ctor.fields.isEmpty) {
      typeExpr(
        definition.name.as("Function"),
        Seq(
          typeExpr(definition.name.as("Unit")),
          typeExpr(definition.name.as(resultParamName))
        )
      )
    } else {
      ctor.fields.foldRight(typeExpr(definition.name.as(resultParamName))) { (field, acc) =>
        typeExpr(definition.name.as("Function"), Seq(field.typeExpression, acc))
      }
    }

  /** Helper to create a sourced type expression from a name and optional generic arguments. */
  private def typeExpr(
      name: Sourced[String],
      genericArgs: Seq[Sourced[SourceExpression]] = Seq.empty
  ): Sourced[SourceExpression] =
    name.as(SourceExpression.FunctionApplication(None, name, genericArgs, Seq.empty))
}

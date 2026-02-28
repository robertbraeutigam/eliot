package com.vanillasource.eliot.eliotc.matchdesugar.processor

import com.vanillasource.eliot.eliotc.core.fact.{Expression => CoreExpression, QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleValue

object ConstructorTypeAnalyzer {

  /** Count the number of fields a constructor has from its type signature. The type has the form:
    * GenParam -> ... -> Function^Type(Field1, Function^Type(Field2, ... DataType)). We skip leading
    * FunctionLiterals (generic params) then count Function^Type applications.
    */
  def countConstructorFields(umv: UnifiedModuleValue): Int =
    countFieldsInType(umv.namedValue.typeStack.signature)

  def extractDataTypeName(expr: CoreExpression): Option[String] =
    expr match {
      case CoreExpression.FunctionLiteral(_, _, body) => extractDataTypeName(body.value.signature)
      case _ =>
        asFunctionTypeReturnType(expr) match {
          case Some(returnType) => extractDataTypeName(returnType)
          case None             => findTypeConstructorName(expr)
        }
    }

  private def countFieldsInType(expr: CoreExpression): Int =
    expr match {
      case CoreExpression.FunctionLiteral(_, _, body) => countFieldsInType(body.value.signature)
      case _                                          => asFunctionTypeReturnType(expr).fold(0)(ret => 1 + countFieldsInType(ret))
    }

  private def findTypeConstructorName(expr: CoreExpression): Option[String] =
    expr match {
      case CoreExpression.NamedValueReference(qn, _, _) if qn.value.qualifier == Qualifier.Type && qn.value.name != "Function" =>
        Some(qn.value.name)
      case CoreExpression.FunctionApplication(target, _) =>
        findTypeConstructorName(target.value.signature)
      case _ => None
    }

  /** If expr is a Function type application Function(paramType)(returnType), returns the return type. */
  private def asFunctionTypeReturnType(expr: CoreExpression): Option[CoreExpression] =
    expr match {
      case CoreExpression.FunctionApplication(target, arg) =>
        target.value.signature match {
          case CoreExpression.FunctionApplication(innerTarget, _) =>
            innerTarget.value.signature match {
              case CoreExpression.NamedValueReference(qn, _, _) if qn.value == QualifiedName("Function", Qualifier.Type) =>
                Some(arg.value.signature)
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
}

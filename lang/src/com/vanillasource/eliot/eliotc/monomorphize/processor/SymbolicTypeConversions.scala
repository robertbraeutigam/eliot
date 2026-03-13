package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.unsourced as evUnsourced
import com.vanillasource.eliot.eliotc.eval.fact.{Types, Value}
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicType
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicType.*

object SymbolicTypeConversions {

  def toExpressionValue(st: SymbolicType): ExpressionValue =
    st match {
      case TypeReference(vfqn)          => ExpressionValue.ConcreteValue(Types.dataType(vfqn))
      case TypeVariable(name)           => ExpressionValue.ParameterReference(name, Value.Type)
      case TypeLambda(name, body)       =>
        ExpressionValue.FunctionLiteral(name, Value.Type, evUnsourced(toExpressionValue(body.value)))
      case TypeApplication(target, arg) =>
        ExpressionValue.FunctionApplication(
          evUnsourced(toExpressionValue(target.value)),
          evUnsourced(toExpressionValue(arg.value))
        )
      case LiteralType(value, typeFQN)  =>
        ExpressionValue.ConcreteValue(Value.Direct(value, Types.dataType(typeFQN)))
    }

  def fromExpressionValue(ev: ExpressionValue): SymbolicType =
    ev match {
      case ExpressionValue.ConcreteValue(Value.Type)                   => TypeReference(Types.typeFQN)
      case ExpressionValue.ConcreteValue(Value.Direct(value, vt))      =>
        vt.typeFQN match {
          case Some(fqn) => LiteralType(value, fqn)
          case None      => LiteralType(value, Types.typeFQN)
        }
      case ExpressionValue.ConcreteValue(v)                            =>
        v.typeFQN match {
          case Some(fqn) => TypeReference(fqn)
          case None      => TypeReference(Types.typeFQN)
        }
      case ExpressionValue.ParameterReference(name, _)                 => TypeVariable(name)
      case ExpressionValue.FunctionLiteral(name, _, body)              =>
        TypeLambda(name, unsourced(fromExpressionValue(body.value)))
      case ExpressionValue.FunctionApplication(target, arg)            =>
        TypeApplication(unsourced(fromExpressionValue(target.value)), unsourced(fromExpressionValue(arg.value)))
      case ExpressionValue.NativeFunction(_, _)                        =>
        TypeReference(Types.typeFQN)
    }
}

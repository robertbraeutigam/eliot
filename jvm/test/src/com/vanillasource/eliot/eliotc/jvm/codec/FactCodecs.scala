package com.vanillasource.eliot.eliotc.jvm.codec

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec

/** One named [[FactCodec]] instance per type reachable from a compiler fact — the coverage half of
  * `docs/incremental-compilation.md` §13's first step, and the thing that makes coverage a fact rather than a hope:
  * the *compiler* proves it, because a field whose type has no instance does not compile.
  *
  * Reading this file: the [[registry]] at the bottom is the answer — every fact type that has an encoding, and
  * [[declined]] beside it, the ones that deliberately do not. Everything above is the transitive closure of what
  * those need, one derived line per type. It is bulk, not design: nothing here states a shape, because
  * `FactCodec.derived` reads each shape from the compiler's own `Mirror`.
  *
  * Two entries are **hand-written**, and both mark a place where the fact model is untyped or reference-compared —
  * `GroundValue.Direct`'s `Any` payload and `ClassFile`'s `Array[Byte]`. They are written out here rather than given
  * general instances so the decision stays at the one place it applies; each carries its own explanation.
  *
  * This lives in `test` on purpose: step 1 is a measurement, so nothing about a real build changes. If the numbers
  * hold (§14), step 2 moves these instances onto the types themselves as `derives FactCodec`, which is where the
  * "decision belongs to the type" rule (§6 Step 1) wants them, and this file disappears.
  */
object FactCodecs {
  given codec_com_vanillasource_eliot_eliotc_uncurry_fact_MonomorphicParameterDefinition: FactCodec[com.vanillasource.eliot.eliotc.uncurry.fact.MonomorphicParameterDefinition] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_uncurry_fact_UncurriedMonomorphicExpression: FactCodec[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_uncurry_fact_UncurriedMonomorphicExpression_Expression: FactCodec[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.Expression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_uncurry_fact_UncurriedMonomorphicExpression_FunctionApplication: FactCodec[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.FunctionApplication] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_uncurry_fact_UncurriedMonomorphicExpression_FunctionLiteral: FactCodec[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.FunctionLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_uncurry_fact_UncurriedMonomorphicExpression_IntegerLiteral: FactCodec[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.IntegerLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_uncurry_fact_UncurriedMonomorphicExpression_StringLiteral: FactCodec[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.StringLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_uncurry_fact_UncurriedMonomorphicExpression_ParameterReference: FactCodec[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.ParameterReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_uncurry_fact_UncurriedMonomorphicExpression_MonomorphicValueReference: FactCodec[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.MonomorphicValueReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_uncurry_fact_UncurriedMonomorphicValue: FactCodec[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_uncurry_fact_UncurriedMonomorphicValue_Key: FactCodec[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_effect_EffectRowRendering_Layer[A: FactCodec]: FactCodec[com.vanillasource.eliot.eliotc.effect.EffectRowRendering.Layer[A]] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_processor_ValueResolverScope: FactCodec[com.vanillasource.eliot.eliotc.resolve.processor.ValueResolverScope] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_AbilityFQN: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression_FunctionApplication: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression.FunctionApplication] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression_IntegerLiteral: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression.IntegerLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression_StringLiteral: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression.StringLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression_ParameterReference: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression.ParameterReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression_ValueReference: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression.ValueReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression_FunctionLiteral: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression.FunctionLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression_FlatExpression: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression.FlatExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression_MatchExpression: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression.MatchExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression_BlockExpression: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression.BlockExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression_MatchCase: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression.MatchCase] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Expression_BlockLine: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Expression.BlockLine] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Pattern: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Pattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Pattern_ConstructorPattern: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Pattern.ConstructorPattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Pattern_VariablePattern: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Pattern.VariablePattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Pattern_WildcardPattern: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Pattern.WildcardPattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_PrecedenceDeclaration: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.PrecedenceDeclaration] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_QualifiedName: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.QualifiedName] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Qualifier: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Qualifier] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Qualifier_Default: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Qualifier.Default.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Qualifier_Type: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Qualifier.Type.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Qualifier_Meta: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Qualifier.Meta.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Qualifier_Ability: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Qualifier.Ability] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_Qualifier_AbilityImplementation: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.Qualifier.AbilityImplementation] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_ResolvedValue: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_ResolvedValue_ResolvedAbilityConstraint: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue.ResolvedAbilityConstraint] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_resolve_fact_ResolvedValue_Key: FactCodec[com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_used_UsedNames: FactCodec[com.vanillasource.eliot.eliotc.used.UsedNames] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_used_UsedNames_Key: FactCodec[com.vanillasource.eliot.eliotc.used.UsedNames.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_used_UsedNames_UsageStats: FactCodec[com.vanillasource.eliot.eliotc.used.UsedNames.UsageStats] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_termination_fact_RecursionCheckedValue: FactCodec[com.vanillasource.eliot.eliotc.termination.fact.RecursionCheckedValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_termination_fact_RecursionCheckedValue_Key: FactCodec[com.vanillasource.eliot.eliotc.termination.fact.RecursionCheckedValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredExpression: FactCodec[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredExpression_FunctionApplication: FactCodec[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression.FunctionApplication] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredExpression_IntegerLiteral: FactCodec[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression.IntegerLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredExpression_StringLiteral: FactCodec[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression.StringLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredExpression_ParameterReference: FactCodec[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression.ParameterReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredExpression_ValueReference: FactCodec[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression.ValueReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredExpression_FunctionLiteral: FactCodec[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression.FunctionLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredExpression_FlatExpression: FactCodec[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression.FlatExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredValue: FactCodec[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredValue_ResolvedAbilityConstraint: FactCodec[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredValue.ResolvedAbilityConstraint] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredValue_Key: FactCodec[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_AST: FactCodec[com.vanillasource.eliot.eliotc.core.fact.AST] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_CoreAST: FactCodec[com.vanillasource.eliot.eliotc.core.fact.CoreAST] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_CoreAST_Key: FactCodec[com.vanillasource.eliot.eliotc.core.fact.CoreAST.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Expression: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Expression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Expression_NamedValueReference: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Expression.NamedValueReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Expression_FunctionApplication: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Expression.FunctionApplication] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Expression_FunctionLiteral: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Expression.FunctionLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Expression_IntegerLiteral: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Expression.IntegerLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Expression_StringLiteral: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Expression.StringLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Expression_FlatExpression: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Expression.FlatExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Expression_MatchExpression: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Expression.MatchExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Expression_BlockExpression: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Expression.BlockExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Expression_MatchCase: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Expression.MatchCase] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Expression_BlockLine: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Expression.BlockLine] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_NamedValue: FactCodec[com.vanillasource.eliot.eliotc.core.fact.NamedValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_NamedValue_CoreAbilityConstraint: FactCodec[com.vanillasource.eliot.eliotc.core.fact.NamedValue.CoreAbilityConstraint] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Pattern: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Pattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Pattern_ConstructorPattern: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Pattern.ConstructorPattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Pattern_VariablePattern: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Pattern.VariablePattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_Pattern_WildcardPattern: FactCodec[com.vanillasource.eliot.eliotc.core.fact.Pattern.WildcardPattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_PrecedenceDeclaration: FactCodec[com.vanillasource.eliot.eliotc.core.fact.PrecedenceDeclaration] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_RoleHint: FactCodec[com.vanillasource.eliot.eliotc.core.fact.RoleHint] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_RoleHint_NoHint: FactCodec[com.vanillasource.eliot.eliotc.core.fact.RoleHint.NoHint.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_RoleHint_ValueConstructor: FactCodec[com.vanillasource.eliot.eliotc.core.fact.RoleHint.ValueConstructor] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_RoleHint_TypeConstructor: FactCodec[com.vanillasource.eliot.eliotc.core.fact.RoleHint.TypeConstructor] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_core_fact_RoleHint_FieldAccessor: FactCodec[com.vanillasource.eliot.eliotc.core.fact.RoleHint.FieldAccessor] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ability_fact_AbilityImplementation: FactCodec[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ability_fact_AbilityImplementation_Resolution: FactCodec[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation.Resolution] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ability_fact_AbilityImplementation_Resolution_Resolved: FactCodec[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation.Resolution.Resolved] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ability_fact_AbilityImplementation_Resolution_NoImplementation: FactCodec[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation.Resolution.NoImplementation.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ability_fact_AbilityImplementation_Resolution_Rejected: FactCodec[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation.Resolution.Rejected] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ability_fact_AbilityImplementation_Resolution_Ambiguous: FactCodec[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation.Resolution.Ambiguous.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ability_fact_AbilityImplementation_Key: FactCodec[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ability_fact_AbilityImplementationCheck: FactCodec[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementationCheck] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ability_fact_AbilityImplementationCheck_Key: FactCodec[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementationCheck.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ability_fact_ModuleAbilityOverlapCheck: FactCodec[com.vanillasource.eliot.eliotc.ability.fact.ModuleAbilityOverlapCheck] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ability_fact_ModuleAbilityOverlapCheck_Key: FactCodec[com.vanillasource.eliot.eliotc.ability.fact.ModuleAbilityOverlapCheck.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ModuleAbilities: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ModuleAbilities] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ModuleAbilities_DeclaredMethod: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ModuleAbilities.DeclaredMethod] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ModuleAbilities_Impl: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ModuleAbilities.Impl] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ModuleAbilities_Key: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ModuleAbilities.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ModuleConstructors: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ModuleConstructors] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ModuleConstructors_Key: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ModuleConstructors.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ModuleName: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ModuleName] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ModuleNames: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ModuleNames] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ModuleNames_Key: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ModuleNames.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ModuleValue: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ModuleValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ModuleValue_Key: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ModuleValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_QualifiedName: FactCodec[com.vanillasource.eliot.eliotc.module.fact.QualifiedName] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_Qualifier: FactCodec[com.vanillasource.eliot.eliotc.module.fact.Qualifier] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_Qualifier_Default: FactCodec[com.vanillasource.eliot.eliotc.module.fact.Qualifier.Default.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_Qualifier_Type: FactCodec[com.vanillasource.eliot.eliotc.module.fact.Qualifier.Type.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_Qualifier_Meta: FactCodec[com.vanillasource.eliot.eliotc.module.fact.Qualifier.Meta.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_Qualifier_Ability: FactCodec[com.vanillasource.eliot.eliotc.module.fact.Qualifier.Ability] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_Qualifier_AbilityImplementation: FactCodec[com.vanillasource.eliot.eliotc.module.fact.Qualifier.AbilityImplementation] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_Role: FactCodec[com.vanillasource.eliot.eliotc.module.fact.Role] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_Role_Runtime: FactCodec[com.vanillasource.eliot.eliotc.module.fact.Role.Runtime.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_Role_Signature: FactCodec[com.vanillasource.eliot.eliotc.module.fact.Role.Signature.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_UnifiedModuleNames: FactCodec[com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleNames] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_UnifiedModuleNames_Key: FactCodec[com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleNames.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_UnifiedModuleValue: FactCodec[com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_UnifiedModuleValue_Key: FactCodec[com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_module_fact_ValueFQN: FactCodec[com.vanillasource.eliot.eliotc.module.fact.ValueFQN] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_reconcile_fact_ReconciledMonomorphicExpression: FactCodec[com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_reconcile_fact_ReconciledMonomorphicExpression_Expression: FactCodec[com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicExpression.Expression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_reconcile_fact_ReconciledMonomorphicExpression_FunctionApplication: FactCodec[com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicExpression.FunctionApplication] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_reconcile_fact_ReconciledMonomorphicExpression_FunctionLiteral: FactCodec[com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicExpression.FunctionLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_reconcile_fact_ReconciledMonomorphicExpression_IntegerLiteral: FactCodec[com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicExpression.IntegerLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_reconcile_fact_ReconciledMonomorphicExpression_StringLiteral: FactCodec[com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicExpression.StringLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_reconcile_fact_ReconciledMonomorphicExpression_ParameterReference: FactCodec[com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicExpression.ParameterReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_reconcile_fact_ReconciledMonomorphicExpression_MonomorphicValueReference: FactCodec[com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicExpression.MonomorphicValueReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_reconcile_fact_ReconciledMonomorphicValue: FactCodec[com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_reconcile_fact_ReconciledMonomorphicValue_Key: FactCodec[com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_platform_Platform: FactCodec[com.vanillasource.eliot.eliotc.platform.Platform] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_platform_Platform_Compiler: FactCodec[com.vanillasource.eliot.eliotc.platform.Platform.Compiler.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_platform_Platform_Runtime: FactCodec[com.vanillasource.eliot.eliotc.platform.Platform.Runtime.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_token_SourceTokens: FactCodec[com.vanillasource.eliot.eliotc.token.SourceTokens] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_token_SourceTokens_Key: FactCodec[com.vanillasource.eliot.eliotc.token.SourceTokens.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_token_Token: FactCodec[com.vanillasource.eliot.eliotc.token.Token] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_token_Token_Identifier: FactCodec[com.vanillasource.eliot.eliotc.token.Token.Identifier] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_token_Token_Symbol: FactCodec[com.vanillasource.eliot.eliotc.token.Token.Symbol] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_token_Token_Keyword: FactCodec[com.vanillasource.eliot.eliotc.token.Token.Keyword] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_token_Token_IntegerLiteral: FactCodec[com.vanillasource.eliot.eliotc.token.Token.IntegerLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_token_Token_StringLiteral: FactCodec[com.vanillasource.eliot.eliotc.token.Token.StringLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_file_FileContent: FactCodec[com.vanillasource.eliot.eliotc.source.file.FileContent] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_file_FileContent_Key: FactCodec[com.vanillasource.eliot.eliotc.source.file.FileContent.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_content_SourceContent: FactCodec[com.vanillasource.eliot.eliotc.source.content.SourceContent] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_content_SourceContent_Key: FactCodec[com.vanillasource.eliot.eliotc.source.content.SourceContent.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_content_Sourced[T: FactCodec]: FactCodec[com.vanillasource.eliot.eliotc.source.content.Sourced[T]] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_scan_FilesystemMount: FactCodec[com.vanillasource.eliot.eliotc.source.scan.FilesystemMount] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_scan_PathScan: FactCodec[com.vanillasource.eliot.eliotc.source.scan.PathScan] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_scan_PathScan_Key: FactCodec[com.vanillasource.eliot.eliotc.source.scan.PathScan.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_scan_PoolModules: FactCodec[com.vanillasource.eliot.eliotc.source.scan.PoolModules] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_scan_PoolModules_Key: FactCodec[com.vanillasource.eliot.eliotc.source.scan.PoolModules.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_stat_FileStat: FactCodec[com.vanillasource.eliot.eliotc.source.stat.FileStat] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_source_stat_FileStat_Key: FactCodec[com.vanillasource.eliot.eliotc.source.stat.FileStat.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_block_fact_BlockDesugaredValue: FactCodec[com.vanillasource.eliot.eliotc.block.fact.BlockDesugaredValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_block_fact_BlockDesugaredValue_Key: FactCodec[com.vanillasource.eliot.eliotc.block.fact.BlockDesugaredValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_processor_TokenClassifier_AnnotatedPart: FactCodec[com.vanillasource.eliot.eliotc.operator.processor.TokenClassifier.AnnotatedPart] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_processor_TokenClassifier_Token: FactCodec[com.vanillasource.eliot.eliotc.operator.processor.TokenClassifier.Token] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_processor_TokenClassifier_PrefixOp: FactCodec[com.vanillasource.eliot.eliotc.operator.processor.TokenClassifier.PrefixOp] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_processor_TokenClassifier_PostfixOp: FactCodec[com.vanillasource.eliot.eliotc.operator.processor.TokenClassifier.PostfixOp] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_processor_TokenClassifier_InfixOp: FactCodec[com.vanillasource.eliot.eliotc.operator.processor.TokenClassifier.InfixOp] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_processor_TokenClassifier_Operand: FactCodec[com.vanillasource.eliot.eliotc.operator.processor.TokenClassifier.Operand] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedExpression: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedExpression_FunctionApplication: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.FunctionApplication] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedExpression_IntegerLiteral: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.IntegerLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedExpression_StringLiteral: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.StringLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedExpression_ParameterReference: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.ParameterReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedExpression_ValueReference: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.ValueReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedExpression_FunctionLiteral: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.FunctionLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedExpression_SignatureView: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.SignatureView] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedExpression_SignatureView_Binder: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.SignatureView.Binder] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedValue: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedValue_ResolvedAbilityConstraint: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.ResolvedAbilityConstraint] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedValue_Key: FactCodec[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_saturate_fact_BinderRoles: FactCodec[com.vanillasource.eliot.eliotc.saturate.fact.BinderRoles] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_saturate_fact_BinderRoles_Disposition: FactCodec[com.vanillasource.eliot.eliotc.saturate.fact.BinderRoles.Disposition] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_saturate_fact_BinderRoles_Disposition_CollapseErase: FactCodec[com.vanillasource.eliot.eliotc.saturate.fact.BinderRoles.Disposition.CollapseErase.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_saturate_fact_BinderRoles_Disposition_CollapseToRepresentation: FactCodec[com.vanillasource.eliot.eliotc.saturate.fact.BinderRoles.Disposition.CollapseToRepresentation.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_saturate_fact_BinderRoles_Disposition_Specialize: FactCodec[com.vanillasource.eliot.eliotc.saturate.fact.BinderRoles.Disposition.Specialize.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_saturate_fact_BinderRoles_Role: FactCodec[com.vanillasource.eliot.eliotc.saturate.fact.BinderRoles.Role] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_saturate_fact_SaturatedValue: FactCodec[com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_saturate_fact_SaturatedValue_Key: FactCodec[com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_AST: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.AST] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_ArgumentDefinition: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.ArgumentDefinition] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_DataConstructor: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.DataConstructor] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_DataDefinition: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.DataDefinition] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_EffectRow[C: FactCodec]: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.EffectRow[C]] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_EffectRow_ParameterEffects[C: FactCodec]: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.EffectRow.ParameterEffects[C]] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Expression: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Expression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Expression_FunctionApplication: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Expression.FunctionApplication] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Expression_FunctionLiteral: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Expression.FunctionLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Expression_IntegerLiteral: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Expression.IntegerLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Expression_StringLiteral: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Expression.StringLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Expression_FlatExpression: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Expression.FlatExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Expression_MatchExpression: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Expression.MatchExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Expression_BlockExpression: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Expression.BlockExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Expression_BlockLine: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Expression.BlockLine] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Expression_EffectfulType: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Expression.EffectfulType] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Expression_MatchCase: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Expression.MatchCase] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Fixity: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Fixity] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Fixity_Application: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Fixity.Application.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Fixity_Prefix: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Fixity.Prefix.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Fixity_Infix: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Fixity.Infix] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Fixity_Postfix: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Fixity.Postfix.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Fixity_Associativity: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Fixity.Associativity] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Fixity_Associativity_Left: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Fixity.Associativity.Left.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Fixity_Associativity_Right: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Fixity.Associativity.Right.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Fixity_Associativity_None: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Fixity.Associativity.None.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_FunctionDefinition: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.FunctionDefinition] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_GenericParameter: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.GenericParameter] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_GenericParameter_AbilityConstraint: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.GenericParameter.AbilityConstraint] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_ImportStatement: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.ImportStatement] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_LambdaParameterDefinition: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.LambdaParameterDefinition] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Pattern: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Pattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Pattern_ConstructorPattern: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Pattern.ConstructorPattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Pattern_VariablePattern: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Pattern.VariablePattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Pattern_WildcardPattern: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Pattern.WildcardPattern] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_PrecedenceDeclaration: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.PrecedenceDeclaration] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_PrecedenceDeclaration_Relation: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.PrecedenceDeclaration.Relation] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_PrecedenceDeclaration_Relation_Above: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.PrecedenceDeclaration.Relation.Above.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_PrecedenceDeclaration_Relation_Below: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.PrecedenceDeclaration.Relation.Below.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_PrecedenceDeclaration_Relation_At: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.PrecedenceDeclaration.Relation.At.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_SourceAST: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.SourceAST] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_SourceAST_Key: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.SourceAST.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Visibility: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Visibility] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Visibility_Public: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Visibility.Public.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_fact_Visibility_Private: FactCodec[com.vanillasource.eliot.eliotc.ast.fact.Visibility.Private.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_parser_InputStream[I: FactCodec]: FactCodec[com.vanillasource.eliot.eliotc.ast.parser.InputStream[I]] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_parser_ParserError: FactCodec[com.vanillasource.eliot.eliotc.ast.parser.ParserError] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_parser_ParserResult[A: FactCodec]: FactCodec[com.vanillasource.eliot.eliotc.ast.parser.ParserResult[A]] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_parser_ParserResult_Consume: FactCodec[com.vanillasource.eliot.eliotc.ast.parser.ParserResult.Consume] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_parser_ParserResult_Consume_Consumed: FactCodec[com.vanillasource.eliot.eliotc.ast.parser.ParserResult.Consume.Consumed.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_ast_parser_ParserResult_Consume_NotConsumed: FactCodec[com.vanillasource.eliot.eliotc.ast.parser.ParserResult.Consume.NotConsumed.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_row_RowChecker_RowResult: FactCodec[com.vanillasource.eliot.eliotc.row.RowChecker.RowResult] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_row_RowElaborator_Elaborated: FactCodec[com.vanillasource.eliot.eliotc.row.RowElaborator.Elaborated] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_row_RowElaborator_Violation: FactCodec[com.vanillasource.eliot.eliotc.row.RowElaborator.Violation] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_row_fact_RowElaboratedValue: FactCodec[com.vanillasource.eliot.eliotc.row.fact.RowElaboratedValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_row_fact_RowElaboratedValue_Key: FactCodec[com.vanillasource.eliot.eliotc.row.fact.RowElaboratedValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_namedvalues_fact_NamedValuesIndex: FactCodec[com.vanillasource.eliot.eliotc.namedvalues.fact.NamedValuesIndex] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_namedvalues_fact_NamedValuesIndex_Key: FactCodec[com.vanillasource.eliot.eliotc.namedvalues.fact.NamedValuesIndex.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_namedvalues_fact_NamedValuesRewrittenValue: FactCodec[com.vanillasource.eliot.eliotc.namedvalues.fact.NamedValuesRewrittenValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_namedvalues_fact_NamedValuesRewrittenValue_Key: FactCodec[com.vanillasource.eliot.eliotc.namedvalues.fact.NamedValuesRewrittenValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_channel_EffectAccounting: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccounting] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_channel_EffectAccounting_Key: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccounting.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_channel_RefinementTable: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_channel_RefinementTable_NodeMeta: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable.NodeMeta] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_channel_RefinementTable_Key: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_channel_WovenValue: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.channel.WovenValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_channel_WovenValue_Key: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.channel.WovenValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_domain_SemValue_VType: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.VType.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_domain_SemValue_VConst: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.VConst] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_domain_SemValue_NeutralHead_Param: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.NeutralHead.Param] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_domain_SemValue_NeutralHead_SignatureBinder: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.NeutralHead.SignatureBinder] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_domain_SemValue_Spine_SNil: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.Spine.SNil.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_BindingContribution_Body: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.BindingContribution.Body] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_BodyValueReferences: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.BodyValueReferences] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_BodyValueReferences_Key: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.BodyValueReferences.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_CompilerMonomorphicValue: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.CompilerMonomorphicValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_CompilerMonomorphicValue_Key: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.CompilerMonomorphicValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_GroundValue: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_GroundValue_Structure: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Structure] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_GroundValue_Param: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Param] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_MonomorphicExpression: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_MonomorphicExpression_Expression: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression.Expression] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_MonomorphicExpression_FunctionApplication: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression.FunctionApplication] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_MonomorphicExpression_FunctionLiteral: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression.FunctionLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_MonomorphicExpression_IntegerLiteral: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression.IntegerLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_MonomorphicExpression_StringLiteral: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression.StringLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_MonomorphicExpression_ParameterReference: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression.ParameterReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_MonomorphicExpression_MonomorphicValueReference: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression.MonomorphicValueReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_MonomorphicValue: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_MonomorphicValue_Key: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_check_CheckState_CarrierHead: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.check.CheckState.CarrierHead] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_check_CheckState_CarrierHead_TopDef: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.check.CheckState.CarrierHead.TopDef] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_check_CheckState_CarrierHead_Meta: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.check.CheckState.CarrierHead.Meta] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_check_SemExpression_IntegerLiteral: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.check.SemExpression.IntegerLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_check_SemExpression_StringLiteral: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.check.SemExpression.StringLiteral] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_check_SemExpression_ParameterReference: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.check.SemExpression.ParameterReference] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_check_Track: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.check.Track] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_check_Track_Runtime: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.check.Track.Runtime.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_check_Track_Compiler: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.check.Track.Compiler.type] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_monomorphize_check_TypeStackLoop_Result: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.check.TypeStackLoop.Result] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_pos_Position: FactCodec[com.vanillasource.eliot.eliotc.pos.Position] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_pos_PositionRange: FactCodec[com.vanillasource.eliot.eliotc.pos.PositionRange] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_compiler_cache_JavaSerializationCacheBackend: FactCodec[com.vanillasource.eliot.eliotc.compiler.cache.JavaSerializationCacheBackend] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_compiler_cache_OutputFileStat: FactCodec[com.vanillasource.eliot.eliotc.compiler.cache.OutputFileStat] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_compiler_cache_OutputFileStat_Key: FactCodec[com.vanillasource.eliot.eliotc.compiler.cache.OutputFileStat.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_compiler_cache_UpToDate: FactCodec[com.vanillasource.eliot.eliotc.compiler.cache.UpToDate] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_compiler_cache_UpToDate_Key: FactCodec[com.vanillasource.eliot.eliotc.compiler.cache.UpToDate.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_feedback_CompilerError: FactCodec[com.vanillasource.eliot.eliotc.feedback.CompilerError] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_jvm_jargen_GenerateExecutableJar: FactCodec[com.vanillasource.eliot.eliotc.jvm.jargen.GenerateExecutableJar] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_jvm_jargen_GenerateExecutableJar_Key: FactCodec[com.vanillasource.eliot.eliotc.jvm.jargen.GenerateExecutableJar.Key] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_jvm_classgen_processor_NativeImplementation_GenericNativeSignature: FactCodec[com.vanillasource.eliot.eliotc.jvm.classgen.processor.NativeImplementation.GenericNativeSignature] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_jvm_classgen_processor_TypeState: FactCodec[com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_jvm_classgen_asm_JvmIdentifier: FactCodec[com.vanillasource.eliot.eliotc.jvm.classgen.asm.JvmIdentifier] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_jvm_classgen_fact_GeneratedModule: FactCodec[com.vanillasource.eliot.eliotc.jvm.classgen.fact.GeneratedModule] = FactCodec.derived
  given codec_com_vanillasource_eliot_eliotc_jvm_classgen_fact_GeneratedModule_Key: FactCodec[com.vanillasource.eliot.eliotc.jvm.classgen.fact.GeneratedModule.Key] = FactCodec.derived
  /** Hand-written because `GroundValue.Direct(value: Any, ...)` has an untyped payload slot: no codec can be derived
    * for `Any`. At runtime the slot holds exactly one of three shapes (see `PostDrainQuoter`'s scaladoc: `BigInt`,
    * `String`, `Boolean`), so this writes a discriminating tag and fails loudly on anything else — the encoding the
    * eventual `Direct(value: Literal)` refactor would produce, measured now without doing the refactor.
    */
  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_GroundValue_Direct
      : FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Direct] =
    new FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Direct] {
      override def write(
          out: FactCodec.Output,
          value: com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Direct
      ): Unit = {
        value.value match {
          case v: BigInt  => out.raw.writeByte(0); FactCodec[BigInt].write(out, v)
          case v: String  => out.raw.writeByte(1); FactCodec[String].write(out, v)
          case v: Boolean => out.raw.writeByte(2); FactCodec[Boolean].write(out, v)
          case other      => throw new IllegalStateException(s"Untyped GroundValue.Direct payload: ${other.getClass}")
        }
        summon[FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue]].write(out, value.valueType)
      }

      override def read(in: FactCodec.Input): com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Direct = {
        val payload: Any = in.raw.readByte() match {
          case 0 => FactCodec[BigInt].read(in)
          case 1 => FactCodec[String].read(in)
          case 2 => FactCodec[Boolean].read(in)
          case t => throw new IllegalStateException(s"Unknown GroundValue.Direct payload tag: $t")
        }
        com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Direct(
          payload,
          summon[FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue]].read(in)
        )
      }
    }

  /** Hand-written because `ClassFile.bytecode` is a bare `Array[Byte]`, and there is deliberately no
    * `FactCodec[Array[Byte]]`: an array *encodes* fine and compares by **reference**, which is exactly the §3.1 /
    * §4 defect (`read(write(v))` never equals `recompute(v)`). Writing the codec here rather than as a general
    * instance keeps that decision at the one place it applies. This makes `GeneratedModule` **encodable**; it does not
    * make it **equality-stable** — that needs `ClassFile` to carry a value-comparable representation, and the
    * conformance harness reports it as a law failure until it does.
    */
  given codec_com_vanillasource_eliot_eliotc_jvm_classgen_fact_ClassFile
      : FactCodec[com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile] =
    new FactCodec[com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile] {
      override def write(
          out: FactCodec.Output,
          value: com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
      ): Unit = {
        FactCodec[String].write(out, value.fileName)
        FactCodec[Seq[Byte]].write(out, value.bytecode.toSeq)
      }

      override def read(in: FactCodec.Input): com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile =
        com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile(
          FactCodec[String].read(in),
          FactCodec[Seq[Byte]].read(in).toArray
        )
    }

  given codec_com_vanillasource_eliot_eliotc_monomorphize_fact_GroundValue_Type: FactCodec[com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Type.type] = FactCodec.singleton(com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.Type)

  /** Every compiler fact type that has an encoding, keyed by its runtime class. The two absentees are the
    * declines: `ContributedBinding` and `NativeBinding` carry `SemValue` closures (§3.1, §4). */
  val registry: Map[Class[?], FactCodec[Any]] = Map(
    classOf[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation] -> codec_com_vanillasource_eliot_eliotc_ability_fact_AbilityImplementation.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementationCheck] -> codec_com_vanillasource_eliot_eliotc_ability_fact_AbilityImplementationCheck.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.ability.fact.ModuleAbilityOverlapCheck] -> codec_com_vanillasource_eliot_eliotc_ability_fact_ModuleAbilityOverlapCheck.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.ast.fact.SourceAST] -> codec_com_vanillasource_eliot_eliotc_ast_fact_SourceAST.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.block.fact.BlockDesugaredValue] -> codec_com_vanillasource_eliot_eliotc_block_fact_BlockDesugaredValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.compiler.cache.OutputFileStat] -> codec_com_vanillasource_eliot_eliotc_compiler_cache_OutputFileStat.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.compiler.cache.UpToDate] -> codec_com_vanillasource_eliot_eliotc_compiler_cache_UpToDate.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.core.fact.CoreAST] -> codec_com_vanillasource_eliot_eliotc_core_fact_CoreAST.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.jvm.classgen.fact.GeneratedModule] -> codec_com_vanillasource_eliot_eliotc_jvm_classgen_fact_GeneratedModule.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.jvm.jargen.GenerateExecutableJar] -> codec_com_vanillasource_eliot_eliotc_jvm_jargen_GenerateExecutableJar.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredValue] -> codec_com_vanillasource_eliot_eliotc_matchdesugar_fact_MatchDesugaredValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.module.fact.ModuleAbilities] -> codec_com_vanillasource_eliot_eliotc_module_fact_ModuleAbilities.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.module.fact.ModuleConstructors] -> codec_com_vanillasource_eliot_eliotc_module_fact_ModuleConstructors.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.module.fact.ModuleNames] -> codec_com_vanillasource_eliot_eliotc_module_fact_ModuleNames.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.module.fact.ModuleValue] -> codec_com_vanillasource_eliot_eliotc_module_fact_ModuleValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleNames] -> codec_com_vanillasource_eliot_eliotc_module_fact_UnifiedModuleNames.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleValue] -> codec_com_vanillasource_eliot_eliotc_module_fact_UnifiedModuleValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccounting] -> codec_com_vanillasource_eliot_eliotc_monomorphize_channel_EffectAccounting.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable] -> codec_com_vanillasource_eliot_eliotc_monomorphize_channel_RefinementTable.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.monomorphize.channel.WovenValue] -> codec_com_vanillasource_eliot_eliotc_monomorphize_channel_WovenValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.monomorphize.fact.BodyValueReferences] -> codec_com_vanillasource_eliot_eliotc_monomorphize_fact_BodyValueReferences.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.monomorphize.fact.CompilerMonomorphicValue] -> codec_com_vanillasource_eliot_eliotc_monomorphize_fact_CompilerMonomorphicValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue] -> codec_com_vanillasource_eliot_eliotc_monomorphize_fact_MonomorphicValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.namedvalues.fact.NamedValuesIndex] -> codec_com_vanillasource_eliot_eliotc_namedvalues_fact_NamedValuesIndex.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.namedvalues.fact.NamedValuesRewrittenValue] -> codec_com_vanillasource_eliot_eliotc_namedvalues_fact_NamedValuesRewrittenValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue] -> codec_com_vanillasource_eliot_eliotc_operator_fact_OperatorResolvedValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicValue] -> codec_com_vanillasource_eliot_eliotc_reconcile_fact_ReconciledMonomorphicValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue] -> codec_com_vanillasource_eliot_eliotc_resolve_fact_ResolvedValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.row.fact.RowElaboratedValue] -> codec_com_vanillasource_eliot_eliotc_row_fact_RowElaboratedValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue] -> codec_com_vanillasource_eliot_eliotc_saturate_fact_SaturatedValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.source.content.SourceContent] -> codec_com_vanillasource_eliot_eliotc_source_content_SourceContent.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.source.file.FileContent] -> codec_com_vanillasource_eliot_eliotc_source_file_FileContent.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.source.scan.PathScan] -> codec_com_vanillasource_eliot_eliotc_source_scan_PathScan.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.source.scan.PoolModules] -> codec_com_vanillasource_eliot_eliotc_source_scan_PoolModules.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.source.stat.FileStat] -> codec_com_vanillasource_eliot_eliotc_source_stat_FileStat.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.termination.fact.RecursionCheckedValue] -> codec_com_vanillasource_eliot_eliotc_termination_fact_RecursionCheckedValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.token.SourceTokens] -> codec_com_vanillasource_eliot_eliotc_token_SourceTokens.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicValue] -> codec_com_vanillasource_eliot_eliotc_uncurry_fact_UncurriedMonomorphicValue.asInstanceOf[FactCodec[Any]],
    classOf[com.vanillasource.eliot.eliotc.used.UsedNames] -> codec_com_vanillasource_eliot_eliotc_used_UsedNames.asInstanceOf[FactCodec[Any]],
  )

  /** Fact types deliberately not encodable, with the reason. */
  val declined: Map[String, String] = Map(
    "com.vanillasource.eliot.eliotc.monomorphize.fact.ContributedBinding" -> "carries a SemValue closure (a function field), which cannot be equality-stable",
    "com.vanillasource.eliot.eliotc.monomorphize.fact.NativeBinding" -> "carries a SemValue closure (a function field), which cannot be equality-stable",
  )
}

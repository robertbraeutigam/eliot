package com.vanillasource.eliot.eliotc.module2.processor

import com.vanillasource.eliot.eliotc.core.fact.CoreAST
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.module2.fact.ModuleNames

class ModuleNamesProcessor extends TransformationProcessor[CoreAST.Key, ModuleNames.Key](key => CoreAST.Key(key.file)) {

  override protected def generateFromKeyAndFact(
      key: ModuleNames.Key,
      fact: CoreAST
  ): CompilerIO[ModuleNames] = ???
}

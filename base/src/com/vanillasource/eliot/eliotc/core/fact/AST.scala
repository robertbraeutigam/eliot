package com.vanillasource.eliot.eliotc.core.fact

import com.vanillasource.eliot.eliotc.ast.fact.ImportStatement

case class AST(
    importStatements: Seq[ImportStatement],
)

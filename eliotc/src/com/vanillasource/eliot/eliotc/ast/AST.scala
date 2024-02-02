package com.vanillasource.eliot.eliotc.ast

case class AST(importStatements: Seq[ImportStatement], functionDefinitions: Seq[FunctionDefinition])

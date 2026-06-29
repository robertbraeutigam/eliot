package com.vanillasource.eliot.eliotc.apidoc.model

import com.vanillasource.eliot.eliotc.module.fact.ModuleName

/** A documented module: its fully-qualified name, optional module-level documentation, and its documented items
  * (already ordered for presentation — types, then abilities, then values).
  */
case class DocModule(name: ModuleName, doc: Option[String], items: Seq[DocItem]) {

  /** The dotted, fully-qualified title, e.g. `eliot.lang.Int` (or just `HelloWorld` for an unpackaged module). */
  def title: String = (name.packages :+ name.name).mkString(".")

  /** The relative output file name for this module's page. */
  def fileName: String = s"$title.html"
}

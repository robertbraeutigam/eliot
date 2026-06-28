package com.vanillasource.eliot.eliotc.lsp.index

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleValue, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue

import java.net.URI
import java.nio.file.Paths

/** In-scope-name index for completion: maps a document to the value / type / ability-method names visible in it.
  *
  * Completion needs to answer "what names may the user write here". The module system already computes exactly that as
  * [[ModuleValue.dictionary]] — the per-file map of every in-scope [[QualifiedName]] (the file's own definitions, the
  * public names of imported modules, and the ambient system modules) to its [[ValueFQN]]. This index groups those
  * dictionaries by document URI (every value in a file shares one dictionary) and pairs each name with the signature of
  * its [[ResolvedValue]], when one was materialised, for the completion item's detail.
  *
  * Unlike the editor's lexical word-completion, this offers names that are genuinely in scope — including ones the user
  * has never typed (e.g. a freshly importable stdlib function) — and never offers a word that merely happens to appear
  * in the buffer but is out of scope. Client-side filtering narrows the returned list to the typed prefix.
  */
final class CompletionIndex private (
    scopesByUri: Map[String, Map[QualifiedName, ValueFQN]],
    signatures: Map[ValueFQN, ResolvedValue]
) {

  /** Every name in scope in the given document, one entry per distinct written name, sorted by name. */
  def completionsAt(uri: URI): Seq[CompletionIndex.Entry] =
    scopesByUri
      .getOrElse(CompletionIndex.uriKey(uri), Map.empty)
      .toSeq
      .filter((qualifiedName, _) => CompletionIndex.isCompletable(qualifiedName.qualifier))
      .groupBy((qualifiedName, _) => qualifiedName.name)
      .toSeq
      .map((name, named) => CompletionIndex.entryOf(name, named, signatures))
      .sortBy(_.name)
}

object CompletionIndex {

  /** The broad category of a name, used to pick the completion item's icon. */
  enum Kind {
    case Value, Type, AbilityMethod
  }

  /** A single completion candidate: the bare name the user writes, its kind, and an optional rendered type signature. */
  case class Entry(name: String, kind: Kind, detail: Option[String])

  val empty: CompletionIndex = new CompletionIndex(Map.empty, Map.empty)

  /** Build the index from the workspace's [[ModuleValue]] facts (their dictionaries are the in-scope names) and the
    * [[ResolvedValue]] facts (their signatures supply each entry's detail). Both are materialised by every compile.
    */
  def build(moduleValues: Seq[ModuleValue], resolvedValues: Seq[ResolvedValue]): CompletionIndex =
    new CompletionIndex(
      moduleValues.groupBy(value => uriKey(value.uri)).view.mapValues(values => values.flatMap(_.dictionary).toMap).toMap,
      resolvedValues.map(value => value.vfqn -> value).toMap
    )

  /** Ability-implementation marker functions are synthetic (the programmer never references them by name); every other
    * dictionary entry is a name the user can write.
    */
  private def isCompletable(qualifier: Qualifier): Boolean = qualifier match {
    case _: Qualifier.AbilityImplementation => false
    case _                                  => true
  }

  private def entryOf(
      name: String,
      named: Seq[(QualifiedName, ValueFQN)],
      signatures: Map[ValueFQN, ResolvedValue]
  ): Entry =
    Entry(name, kindOf(named.map((qualifiedName, _) => qualifiedName.qualifier)), detailOf(named, signatures))

  /** A name's kind: a type if any of its qualifiers is the type namespace, an ability method if it is an ability member,
    * otherwise an ordinary value/function.
    */
  private def kindOf(qualifiers: Seq[Qualifier]): Kind =
    if (qualifiers.contains(Qualifier.Type)) Kind.Type
    else if (qualifiers.exists { case _: Qualifier.Ability => true; case _ => false }) Kind.AbilityMethod
    else Kind.Value

  /** Render the entry's type signature, preferring the value-level (`Default`) definition and falling back to any other
    * qualifier sharing the name; `None` when no [[ResolvedValue]] for the name was materialised.
    */
  private def detailOf(named: Seq[(QualifiedName, ValueFQN)], signatures: Map[ValueFQN, ResolvedValue]): Option[String] =
    named
      .sortBy((qualifiedName, _) => if (qualifiedName.qualifier == Qualifier.Default) 0 else 1)
      .flatMap((_, fqn) => signatures.get(fqn))
      .headOption
      .map(value => value.typeStack.value.signature.show)

  /** Normalise a URI to a stable key so the editor's `file:///…` URIs match the compiler's `file:/…` URIs. Since CP1.5
    * every source URI is a `file:` URI (the base/platform layers are filesystem roots too, not `jar:` classpath
    * resources); the catch is a defensive fallback for any unexpected non-`file:` URI, which then simply never matches a
    * workspace document. Mirrors [[PositionIndex.uriKey]].
    */
  private def uriKey(uri: URI): String =
    try Paths.get(uri).toString
    catch { case _: IllegalArgumentException | _: java.nio.file.FileSystemNotFoundException => uri.toString }
}

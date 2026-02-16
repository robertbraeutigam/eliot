package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.indent.LayoutToken
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A block is one sequence of tokens that belongs together. It is the atomic unit of parser recovery. One block can
  * only issue one error and then the parser exists. So the finer the blocks the more recovery options there are for the
  * parser.
  */
object Block {

  /** Split the tokens into blocks based on indentation. Note: if the indentation is not well-formed, this method will
    * not issue an error, but will return garbage.
    * @return
    *   A sequence of blocks (which are sequences of tokens).
    */
  def blocks(layoutTokens: Seq[Sourced[LayoutToken]]): Seq[Seq[Sourced[LayoutToken]]] = {
    layoutTokens
      .foldLeft((0, Seq.empty)) { (acc, token) =>
        if (token.value === LayoutToken.Indent) {
          // Increase indent level. Increase is always included in the current block.
          (acc._1 + 1, addToCurrentBlock(acc._2, token))
        } else if (token.value === LayoutToken.Dedent) {
          // We dedent, decrease the indent level, if we hit 0, start a new block
          if (acc._1 === 1) {
            (0, acc._2 :+ Seq(token))
          } else {
            (acc._1 - 1, addToCurrentBlock(acc._2, token))
          }
        } else {
          // Some token, include it in the current sequence
          (acc._1, addToCurrentBlock(acc._2, token))
        }
      }
      ._2
  }

  private def addToCurrentBlock(
      currentBlocks: Seq[Seq[Sourced[LayoutToken]]],
      item: Sourced[LayoutToken]
  ): Seq[Seq[Sourced[LayoutToken]]] =
    if (currentBlocks.isEmpty) {
      Seq(Seq(item))
    } else {
      currentBlocks.updated(currentBlocks.length - 1, currentBlocks.last :+ item)
    }
}

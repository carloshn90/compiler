package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import parser.grammar.GrammarResult.GrammarResult

object ParserStmt {
  def createSemicolonError(line: Int): GrammarResult[Stmt] =
    Left(ErrorCompiler(line - 1, "Expect ';' after value."))
}

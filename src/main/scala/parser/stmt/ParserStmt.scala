package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import parser.grammar.GrammarResult.GrammarResult

object ParserStmt {
  def createSemicolonError(line: Int): GrammarResult[Stmt] =
    Left(ErrorCompiler(if (line > 1) line - 1 else  line, "Expect ';' after value."))
}

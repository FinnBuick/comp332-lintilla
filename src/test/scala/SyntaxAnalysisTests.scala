/*
 * This file is part of COMP332 Assignment 2 2018.
 *
 * Lintilla, a simple functional programming language.
 *
 * © 2018, Dominic Verity and Anthony Sloane, Macquarie University.
 *         All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Tests of the parser of the Lintilla language.
 */

package lintilla

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * Tests that check that the parser works correctly.  I.e., it accepts correct
  * input and produces the appropriate trees, and it rejects illegal input.
  */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

  import LintillaTree._

  val parsers = new SyntaxAnalysis(positions)
  import parsers._

  // Tests of parsing terminals

  test("parsing an identifier of one letter produces the correct tree") {
    identifier("x") should parseTo[String]("x")
  }

  test("parsing an identifier as an applied instance produces the correct tree") {
    idnuse("count") should parseTo[IdnUse](IdnUse("count"))
  }

  test("parsing an identifier containing digits and underscores produces the correct tree") {
    idndef("x1_2_3") should parseTo[IdnDef](IdnDef("x1_2_3"))
  }

  test("parsing an integer as an identifier gives an error") {
    identifier("42") should
    failParseAt(1, 1,
                "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '4' found")
  }

  test("parsing a non-identifier as an identifier gives an error (digit)") {
    identifier("4foo") should
    failParseAt(1, 1,
                "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '4' found")
  }

  test("parsing a non-identifier as an identifier gives an error (underscore)") {
    identifier("_f3") should
    failParseAt(1, 1,
                "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '_' found")
  }

  test("parsing a keyword as an identifier gives an error") {
    identifier("let ") should failParseAt(1, 1,
                                          "identifier expected but keyword found")
  }

  test("parsing a keyword prefix as an identifier produces the correct tree") {
    identifier("letter") should parseTo[String]("letter")
  }

  test("parsing an integer of one digit as an integer produces the correct tree") {
    integer("8") should parseTo[String]("8")
  }

  test("parsing an integer as an integer produces the correct tree") {
    integer("99") should parseTo[String]("99")
  }

  test("parsing a non-integer as an integer gives an error") {
    integer("total") should
    failParseAt(1, 1,
                "string matching regex '[0-9]+' expected but 't' found")
  }

  // FIXME Add tests of your parsers.

  test("parsing an integer as an IntExp produces the correct tree") {
    intexp("42") should parseTo[IntExp](IntExp(42))
  }

  test("parsing a true boolean as a BoolExp produces the correct tree") {
    boolexp("true") should parseTo[BoolExp](BoolExp(true))
  }

  test("parsing a false boolean as a BoolExp produces the correct tree") {
    boolexp("false") should parseTo[BoolExp](BoolExp(false))
  }

  test("parsing an empty block expression produces the correct tree") {
    block("{}") should parseTo[Block](Block(Vector()))
  }

  test("parsing a block expression containing an integer expression produces the correct tree") {
    block("{ 10 }") should parseTo[Block](Block(Vector(IntExp(10))))
  }

  test("parsing a block expression containing a true boolean expression produces the correct tree") {
    block("{ true }") should parseTo[Block](Block(Vector(BoolExp(true))))
  }

  test("parsing a block expression containing a false boolean expression produces the correct tree") {
    block("{ false }") should parseTo[Block](Block(Vector(BoolExp(false))))
  }

  test("parsing a block expression containing an identifier produces the correct tree") {
    block("{ test }") should parseTo[Block](Block(Vector(IdnExp(IdnUse("test")))))
  }

  test("parsing a let declaration produces the correct tree") {
    let("let a = 32") should parseTo[LetDecl](LetDecl(IdnDef("a"),IntExp(32)))
  }

  test("parsing a let mutable declaration produces the correct tree") {
    letmut("let mut hello = 18") should parseTo[LetMutDecl](LetMutDecl(IdnDef("hello"), IntExp(18)))
  }

  test("parsing a plusexp containing integer literals produces the correct tree") {
    plusexp("2 + 2") should parseTo[PlusExp](PlusExp(IntExp(2), IntExp(2)))
  }

  test("parsing a minusexp containing integer literals produces the correct tree") {
    minusexp("2 - 2") should parseTo[MinusExp](MinusExp(IntExp(2), IntExp(2)))
  }

  test("parsing a starexp containing integer literals produces the correct tree") {
    starexp("2 * 2") should parseTo[StarExp](StarExp(IntExp(2), IntExp(2)))
  }

  test("parsing a slashexp containing integer literals produces the correct tree") {
    slashexp("2 / 2") should parseTo[SlashExp](SlashExp(IntExp(2), IntExp(2)))
  }

  test("parsing a negexp containing an integer literal produces the correct tree") {
    negexp("- 2") should parseTo[NegExp](NegExp(IntExp(2)))
  }

  test("parsing an ifexp produces the correct tree") {
    ifexp("if (a) {a}") should parseTo[IfExp](IfExp(IdnUse("a"), Block(IdnUse("a")), Block()))
  }

}

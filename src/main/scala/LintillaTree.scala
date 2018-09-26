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
 * Source program tree definition.
 */

package lintilla

/**
 * Module containing tree structures for representing Lintilla programs.
 */
object LintillaTree {

  import org.bitbucket.inkytonik.kiama.relation.Tree

  /**
    * A relational tree to handle access to parent and sibling nodes.
    */
  type SourceTree = Tree[SourceNode, Program]

  /**
    * The common supertype of all source tree nodes.
    */
  sealed abstract class SourceNode extends Product

  /**
    * A Lintilla program is a list of expressions. The parser ensures that
    * this list is not empty.
    */
  case class Program(exps : Vector[Expression]) extends SourceNode

  /**
    * Common superclass of expressions.
    */
  sealed abstract class Expression extends SourceNode

  /**
    * A `let` expression declares a new immutable variable and initialises it
    * with the value computed by evaluating an associated expression. The scope
    * of the name it introduces extends from the point of the defining `let` to
    * the end of the enclosing block.
    */
  case class LetDecl(name : IdnDef, exp : Expression) extends Expression

  /**
    * A `let mut` expression declares a new mutable variable and initialises it
    * with the value computed by evaluating an associated expression. The scope
    * of the name it introduces extends from the point of the defining `let` to
    * the end of the enclosing block.
    */
  case class LetMutDecl(name : IdnDef, exp : Expression) extends Expression

  /**
    * A `fn` expression declares a new named function. The scope of the name it
    * binds extends throughout the enclosing block, thereby allowing us to define
    * (mutually) recursive functions.
    */
  case class FnDecl(name : IdnDef, args : Vector[ParamDecl],
                    optRet : Option[Type], body : Expression)
      extends Expression

  /**
    * A single parameter declaration.
    */
  case class ParamDecl(idn : IdnDef, tipe : Type) extends Expression

  /**
    * A block comprising a non-empty list of statements.
    */
  case class Block(stmts : Vector[Expression]) extends Expression

  /**
    * Return from the current block, returning value to enclosing expression.
    */
  case class Return(optExp : Option[Expression]) extends Expression

  /**
    * Assignment expression.
    */
  case class AssignExp(idn: IdnUse, exp : Expression) extends Expression

  /**
    * Function application applies the left expression to the right expression.
    */
  case class AppExp(fn : Expression, args : Vector[Expression]) extends Expression

  /**
    * Equality expression compares the left and right expressions for equality.
    */
  case class EqualExp(left : Expression, right : Expression) extends Expression

  /**
    * Less than expression compares the left and right numeric expressions
    *  for less-than order.
    */
  case class LessExp(left : Expression, right : Expression) extends Expression

  /**
    * Addition expression.
    */
  case class PlusExp(left : Expression, right : Expression) extends Expression

  /**
    * Subtraction expression.
    */
  case class MinusExp(left : Expression, right : Expression) extends Expression

  /**
    * Multiplication expression.
    */
  case class StarExp(left : Expression, right : Expression) extends Expression

  /**
    * Integer division expression.
    */
  case class SlashExp(left : Expression, right : Expression) extends Expression

  /**
    * Integer (unary) negation.
    */
  case class NegExp(exp : Expression) extends Expression

  /**
    * Boolean constant expression.
    */
  case class BoolExp(value : Boolean) extends Expression

  /**
    * Named variable expression.
    */
  case class IdnExp(name : IdnUse) extends Expression

  /**
    * Integer constant expression.
    */
  case class IntExp(value : Int) extends Expression

  /**
    * Conditional expression (if). cond is a Boolean condition. The expression
    * evaluates to the value of left (right) if cond is true (false).
    */
  case class IfExp(cond : Expression, left : Expression, right : Expression)
      extends Expression

  /**
    * While loop.
    */
  case class WhileExp(cond : Expression, body : Expression) extends Expression

  /**
    * Common superclass of types.
    */
  sealed abstract class Type extends SourceNode

  /**
    * The basic unit type.
    */
  case class UnitType() extends Type {
    override def toString() = "unit"
  }

  /**
    * The basic integer type.
    */
  case class IntType() extends Type {
    override def toString() = "int"
  }

  /**
    * The basic Boolean type.
    */
  case class BoolType() extends Type {
    override def toString() = "bool"
  }

  /**
    * A function type from an argument of type `argType` to a result of type
    * `resType`.
    */
  case class FnType(argTypes : Vector[Type], resType : Type)
      extends Type {
    override def toString() = s"fn(${argTypes.mkString(",")}) -> ${resType}"
  }

  /**
    * The type of something whose type we cannot determine.  Compatible
    * with anything.
    */
  case class UnknownType() extends Type

  /**
    * An identifier reference.
    */
  sealed abstract class IdnNode extends SourceNode {
    def idn : String
  }

  /**
    * A defining occurrence of an identifier (i.e., a place where an entity named by
    * the identifier is being introduced).
    */
  case class IdnDef(idn : Identifier) extends IdnNode

  /**
    * An applied occurrence (use) of an identifier (i.e., a place where an entity with
    * this name is being used, but not introduced).
    */
  case class IdnUse(idn : Identifier) extends IdnNode

  /**
    * A representation of identifiers as strings.
    */
  type Identifier = String

}

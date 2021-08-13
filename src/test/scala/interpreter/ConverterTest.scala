package org.compiler.example
package interpreter

import error.ErrorCompiler
import interpreter.Converter.{convertToBoolean, convertToDouble}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ConverterTest extends AnyFunSuite with Matchers {

  test("Converter integer to double, should return double value") {

    val result: Either[ErrorCompiler, Double] = convertToDouble(2)(0)(new Environment())._1

    result shouldBe Right(2.0)
  }

  test("Converter double to double, should return double value") {

    val result: Either[ErrorCompiler, Double] = convertToDouble(2.0)(0)(new Environment())._1

    result shouldBe Right(2.0)
  }

  test("Converter string to double, should return double value") {

    val result: Either[ErrorCompiler, Double] = convertToDouble("2")(0)(new Environment())._1

    result shouldBe Right(2.0)
  }

  test("Converter wrong type to double, should return error") {

    val result: Either[ErrorCompiler, Double] = convertToDouble(false)(0)(new Environment())._1

    result shouldBe Left(ErrorCompiler(0, "Impossible to convert false to double"))
  }
  test("Converter wrong value to double, should return error") {

    val result: Either[ErrorCompiler, Double] = convertToDouble("no number")(0)(new Environment())._1

    result shouldBe Left(ErrorCompiler(0, "Impossible to convert no number to double"))
  }

  test("Converter boolean to boolean, should return boolean value") {

    val result: Either[ErrorCompiler, Boolean] = convertToBoolean(false)(0)(new Environment())._1

    result shouldBe Right(false)
  }

  test("Converter string to boolean, should return boolean value") {

    val result: Either[ErrorCompiler, Boolean] = convertToBoolean("true")(0)(new Environment())._1

    result shouldBe Right(true)
  }

  test("Converter wrong type to boolean, should return error") {

    val result: Either[ErrorCompiler, Boolean] = convertToBoolean(2)(0)(new Environment())._1

    result shouldBe Left(ErrorCompiler(0, "Impossible to convert 2 to boolean"))
  }

  test("Converter wrong value to boolean, should return error") {

    val result: Either[ErrorCompiler, Boolean] = convertToBoolean("hello")(0)(new Environment())._1

    result shouldBe Left(ErrorCompiler(0, "Impossible to convert hello to boolean"))
  }

}

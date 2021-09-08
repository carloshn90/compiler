package org.compiler.example
package interpreter

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EnvironmentTest extends AnyFunSuite with Matchers {

  test("Environment assign global value, should update global value") {

    val global: Environment = new Environment().define("a", 0.0)
    val env: Environment = global.createLocalEnv

    val resultEnv: Option[Environment] = env.assign("a", 1.0)

    resultEnv.isDefined shouldBe true
    resultEnv.get.restore.get("a") shouldBe Right(1.0)
  }

  test("Environment assign local value, should local global value") {

    val env: Environment = new Environment().define("a", 0.0)

    val resultEnv: Option[Environment] = env.assign("a", 1.0)

    resultEnv.isDefined shouldBe true
    resultEnv.get.get("a") shouldBe Right(1.0)
    resultEnv.get.restore.get("a") shouldBe Left("Undefined variable 'a'.")
  }

  test("Environment get have to return the less nested value, should local value") {

    val globalEnv: Environment = new Environment().define("a", 0.0)
    val env: Environment = globalEnv.createLocalEnv.define("a", 1.0)

    val resultEnv: Either[String, Any] = env.get("a")

    resultEnv shouldBe Right(1.0)
  }

}

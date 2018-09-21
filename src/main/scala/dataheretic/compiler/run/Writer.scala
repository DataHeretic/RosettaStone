package dataheretic.compiler.run

import dataheretic.compiler.DHCompiler.AST.DataHereticAST
import dataheretic.compiler.run.SimpleRunner.dhTarget
import io.circe.syntax._
import io.circe.generic.auto._
import better.files.Dsl._
import better.files._

import scala.util.{Failure, Success, Try}

class Writer (val dhTarget: File) {

  def writeToFile (astTry: Try[DataHereticAST]): Unit = astTry match {
    case Success(ast) =>
      println (s"> writing to: $dhTarget")
      dhTarget < ast.asJson.spaces2
    case Failure(e) =>
      System.err.println("> Compilation failed. Error:")
      e.printStackTrace(System.err)
  }

}

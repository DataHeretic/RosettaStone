package dataheretic.compiler

object Model {

  case class SqlStatement (str: String) extends AnyVal

  case class SqlTransaction (statements: Seq[SqlStatement])

}
